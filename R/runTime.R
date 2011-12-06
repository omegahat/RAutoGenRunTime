
setGeneric("asReference", function(x, type) standardGeneric("asReference"))


setMethod("asReference", c("ANY", "ANY"),
          function(x, type)
{
  if(!is(x, "RC++Reference"))
     stop("Not an RC++Reference")

    # Can check the type here.
    # 
  if(!is(x, type) && is.na(match(type,  c(class(x), x@classes))))
       stop("reference object does not correspond to target type ", type)

  x  
})

setMethod("asReference", c("NULL", "ANY"), function(x, type) NULL)


# Does this make sense anymore?
setMethod("$", "RC++Reference",
          function(x, name) {
            ..f = get(name, mode = "function")
            structure(function(...)
                        ..f(x, ...),
                       class = "RC++ReferenceMethodFunction")
          })


setMethod("$", "RC++ReferenceUseName", 
           function(x, name) {
                # have to strip off the Ptr from the name of the classes.
                # don't need the classes slot.
                # .ids = paste(gsub("Ptr$", "", c(class(x), x@classes)), name, sep = "_")
             .ids = paste(gsub("Ptr$", "", extends(class(x))), name, sep = "_")        
              for(i in .ids) {
                if(exists(i, mode = "function")) {
                  ..f = get(i, mode = "function")
                 return(structure(function(...) ..f(x, ...), class = "RC++ReferenceMethodFunction"))
                }
              }
             stop("no method for ", name, " in the classes", paste(x@classes, collapse = ", "))
           })


"print.RC++ReferenceMethodFunction" =
function(x)
  print(environment(x)$..f)


# These are the generic methods for accessing an element
# in an external array.
# They can be overridden by auto-generated code
# with more specific  methods for new types of
# derived classes. 
# To use these

if(FALSE) {
setMethod("[[", c("ExternalArray", "numeric"),
          function(x, i, j, ...) {
            if(i < 1)
              stop("index for ", class(x), " must be 1 or more")
              # now go fetch.

            routine = ArrayAccessorElementRoutine(x)
            .Call(routine, x, as.integer(i))
          })


setMethod("[[", c("ExternalArrayWithLength", "numeric"),
          function(x, i, j, ...) {
            if(i < 1)
              stop("index for ", class(x), " must be 1 or more")

            if(i > arrayLength(x))
              stop("index too large for C/C++ array for ", class(x), ". Should be <= ", length(x))
            
            # go get it.
            routine = ArrayAccessorElementRoutine(x)
            .Call(routine, x, as.integer(i))            
          })
}

arrayLength =
function(x)
  x@length

setMethod("length", "ExternalArrayWithLength",
           function(x)
                x@length[1])

setMethod("lapply", "ExternalArrayWithLength",
  function(X, FUN, ...) {
   FUN <- match.fun(FUN)
   lapply(seq(along = X), function(i)  FUN(X[[i]], ...))
 })



  #
  #
setGeneric("ArrayAccessorElementRoutine",
           function(x)
             standardGeneric("ArrayAccessorElementRoutine")
          )



#
# The idea to represent a C/C++-level variable
# in R as a regular R variable is to take its 
# address. But when we use it, we dereference
# it.  This is done most automatically
# when we have a reference. Need more work on basic types. 
#
#  Get a reference
#  valueOf() gives us the derefernced value



# need the value of "to" in the function call at run-time.
setAs("VariableReference", "ANY",
      function(from, to)
         valueOf(from))

setMethod("asReference", "VariableReference",
          function(x, type) {
            x = valueOf(x)
            callNextMethod()
          })


valueOf =
function(obj)
{
  .Call(paste("R_get_valueOf", class(obj@ref), sep = "_"), obj) # class(obj)?
}

copyToR =
  #
  #  version of as(obj, "R version of reference class")
  #  without the user having to know the name of the target R class.
  #
function(obj)
{
  symName = paste("R_coerce", class(obj), gsub("Ptr$", "", class(obj)), sep = "_")
  if(!is.loaded(symName)) {
    if(!is(obj, "RC++Reference")) {
      stop("Don't know how to invoke copyToR for objects not derived (programmatically) from RC++Reference")
    } else 
      stop("No routine ", symName, " loaded to use for copyToR")
  }
  .Call(symName, obj)
}  



setAs("VariableReference", "integer",
       function(from)
          as.integer(valueOf(from)))

setAs("VariableReference", "numeric",
       function(from)
          as.numeric(valueOf(from)))

setAs("VariableReference", "logical",
       function(from)
          as.logical(valueOf(from)))


checkMethodsNumArgs =
  # verify that the R functions provided in ...
  # have the expected number of arguments given in
  # expected
function(expected, ...,  .funs = list(...))
{
  if(length(names(expected)) == 0) {
     if(length(names(.funs)))
       names(expected) = names(.funs)
     else {
       names(expected) = names(.funs) = seq(along = expected)
     }
       
 }
 
  
  ok = sapply(names(expected),
               function(id) {
                 if(typeof(.funs[[id]]) != "closure" && is.na(.funs[[id]]))
                   return(TRUE)

                 f = formals(.funs[[id]])
                 length(f) >= expected[id] || "..." %in% names(f)
               }
             )

  if(any(!ok))
    stop("incorrect number of arguments for ", names(expected)[!ok])

  if(all(sapply(.funs, typeof) != "closure")) {
    warning("all the methods are specified as NA and so none will be set. Ignoring call!")
    return(FALSE)  # don't both calling the function
  }
  
  TRUE
}


new_int =
function(val = 0)
{
   .Call("R_new_int", as.integer(val))
}  




asUnsigned =
function(value, type, force = FALSE)
{
  value = as(value, type)
  if(any(value < 0))
    (if(force) warning else stop)("Need non-negative values")
  
  value
}


fixArrayLength =
  #
  # Perhaps this is more aptly named fixArrayLength.
  # It issues an error or warning and if it is a warning
  # fixes the length to the expected length.
  # This is used when converting an ArrayType to R in coerceRValue in RGCCTranslationUnit
function(obj, expectedLength, fatal = FALSE)
{
  msg = paste("expecting an object of length ", expectedLength, ", got object with length ", length(obj))

  class = if(fatal)
            c("ArrayLengthError", "simpleError", "error", "condition")
          else
            c("ArrayLengthWarning", "simpleWarning", "warning", "condition")
  
  e = structure(list(message = msg, call = NULL, expectedLength = expectedLength),
                 class = class)
  (if(fatal) stop else warning)(e)

  length(obj) = expectedLength
  obj
}



isNativeNull =
  #
  #  checks whether the external pointer associated with x (x or x@ref) is NULL at the C level.
  #  andNULL controls whether the R NULL value is considered a NativeNull also
  #
function(x, andNULL = TRUE)
{
  if(andNULL  && is.null(x))
    return(TRUE)
  
  if(typeof(x) != "externalptr" && !is(x, "RC++Reference"))
    stop("only know how to check for native/C-level NULL for RC++Reference objects and externalptr objects")

  if(is(x, "RC++Reference"))
    x = x@ref

  .Call("R_isNativeNull", x)
}




copyUptoDepth =
function(x, depth = 1)
{
  if(depth == 0)
    return(x)

  ids = names(x)
  structure(lapply(ids,
                   function(id) {
                     cat(class(x), id, class(x[[id, copy = FALSE]]), depth==0, "\n")
                     copyUptoDepth(x[[id, copy = (depth == 0) ]], depth - 1)
                   }), names = ids)
}



setAs("RCReference", "character",
       function(from)
           .Call("R_getStringFromPtr", from@ref))

setAs("externalptr", "character",
       function(from)
           .Call("R_getStringFromPtr", from, TRUE))
