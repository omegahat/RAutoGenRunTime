if(!isGeneric('getRMethods'))
setGeneric("getRMethods",
           function(this) standardGeneric("getRMethods"))

if(!isGeneric('setRMethods'))
setGeneric("setRMethods",
           function(this, ...) standardGeneric("setRMethods"))


setClass('RDerivedClass', representation(base = "character"))

makeDerivedMethodsList =
  #
  # e.g.
  # q = makeDerivedMethodsList(
  #           list("area()" = function(this){cat("In area\n"); 10},
  #                "scale(int)" = function(this, x) {cat("In scale(int)\n")},
  #                "scale(int,int)" = function(this, x, y) { cat("In scale(int, int)\n"); 11},
  #                "new(int,int, int)" = function(){}),
  #           tmpl)
  #
  #
  #
  #
function(methods, template)
{
    # Don't build the list if we have no methods.
    # Makes lookup for degenerate list faster.
  if(is.null(methods))
    return(methods)
  
  if(is.character(template) || is(template, "RDerivedClass"))
    template = getNativeMethodTemplate(template)

 i = pmatch(names(methods), names(template))
 if(any(is.na(i))) {
    # See if it is just spaces. So add a space after a , that doesn't have a space
    # so shape(int,int) becomes shape(int, int).
   j = is.na(i)
   names(methods)[j] = gsub(",([^ ])", ", \\1", names(methods)[j])
   x = pmatch(names(methods)[j], names(template))
   if(any(is.na(x))) {
     bad = names(methods)[j][is.na(x)]
     stop("unrecognized native method names for C++ class: ", paste(bad, collapse = ", "))
   }
   i[j] = x
 }
 template[i] = methods
 template
}  

  

getNativeMethodIds =
function(obj, getTypes = FALSE)
{
  if(is.character(obj)) {
      name = obj
  } else if(is(obj, "RDerivedClass"))
      name = class(obj)
  else
    stop("no suport for getting native method identifiers for classes not sub-classes of RDerivedClass")

  sym = paste("R_getNativeMethodIds", name, sep = "_")
  if(!is.loaded(sym)) 
    stop("No native routine to get method identifiers for class ", name)

  .Call(sym, as.logical(getTypes))
}

getNativeMethodTemplate =
function(obj)
{
  ids = getNativeMethodIds(obj)
  structure(vector("list", length(ids)), names = ids)
}

setGeneric("getDerivedClassMethods",
            function(obj, mergeClassMethods = TRUE, simplify = FALSE) {
              m = standardGeneric("getDerivedClassMethods")

              if(mergeClassMethods && !is(obj, "character")) {
                classMethods = getDerivedClassMethods(class(obj), FALSE)
                if(length(classMethods) == 0)
                  return(m)

                i = sapply(m, is.null)
                j = match(names(m)[i], names(classMethods))
                els = classMethods[j[!is.na(j)]]
                m[names(els)] = els
              }

              if(simplify)
                m[!sapply(m, is.null)]
              else
                m
            })


setMethod("getDerivedClassMethods", "character",
          function(obj, mergeClassMethods = TRUE, simplify = FALSE) {
            sym = paste("R", obj, "get_R_sharedClassMethods", sep = "_")
            if(is.loaded(sym))
              .Call(sym)
            else
              stop("Don't know how to get class-level methods for a C++ derived class named ", obj)
          })

setGeneric("setDerivedClassMethods",
            function(obj, ..., .methods = list(...), .merge = TRUE)
             standardGeneric("setDerivedClassMethods"))


tmp =
  function(obj, ..., .methods = list(...), .merge = TRUE) {
    sym = paste("R", if(is.character(obj)) obj else class(obj), "set_R_sharedClassMethods", sep = "_")
    if(!is.loaded(sym))
      stop("Cannot locate C routine ", sym, " to set shared class methods")


       # to allow the caller to specify the list directly but in the ...
       # rather than the individual elements
    if(length(.methods) == 1 && is.list(.methods[[1]]))
      .methods = .methods[[1]]
    
    template = makeDerivedMethodsList(.methods, obj)

    if(.merge)  {
      current = getDerivedClassMethods(obj)
      if(length(current)) {
        i = match(names(current), names(template))
        i = i[sapply(template[i], is.null) & !(names(template)[i] %in% names(.methods))]
        if(length(i))
          template[i] = current[names(template)[i]]
      }
    }

    if(is.character(obj)) 
       template = template[! sapply(template, is.null)]    

    .Call(sym, template)
    template
  }

setMethod("setDerivedClassMethods", "RDerivedClass", tmp)


setMethod("setDerivedClassMethods", "character", tmp)
