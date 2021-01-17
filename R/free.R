#cat("reading free", isGeneric("Free"), sQuote(find("Free")), "\n")
if(TRUE || !isGeneric("Free")) {
 cat("Definining Free\n")
   setGeneric("Free", function(obj, recursive = FALSE, ...) standardGeneric("Free"))
}

setMethod("Free", "RC++Reference",
           function(obj,  recursive = FALSE, ...) {
              id = gsub("Ref$", "", class(obj))
              sym = paste("R_free_", id, sep = "")  # see getDestructorNames in struct.R in RGCC..
              if(!is.loaded(sym))
                stop("Cannot find default destructor routine callable from R ", sym)

              freeSlots(obj, recursive)

                # now free the top-level object itself.
              .Call(sym, obj)
           })


freeSlots =
function(obj, recursive)
{
   if(is.logical(recursive) && recursive) {
     # determine the slots that are RC++Reference objects
     slots = getSlots(getClass(obj))
     sapply(names(slots),
             function(id) {
               el = slot(obj, id)
               if(is(el, "RC++Reference"))
                 Free(el)
             })
   } else  if(is.character(recursive) && length(recursive)) {
     sapply(recursive,
            function(id) 
              Free(slot(obj, id))
            )
   }
}  



if(TRUE || !isGeneric("addFinalizer"))
  setGeneric("addFinalizer", function(obj, finalizer, default = character(), ...) standardGeneric("addFinalizer"))


setMethod("addFinalizer", c("RC++Reference", "character"),
          function(obj, finalizer, default = character()) {
            sym = getNativeSymbolInfo(finalizer)
            addFinalizer(obj, sym$address)
          })

setMethod("addFinalizer", c("RC++Reference", "logical"),
          function(obj, finalizer, default = character()) {
            if(!finalizer)
              return(NULL)

            if(length(default))
              sym  = default
            else {
              id = gsub("Ref$", "", class(obj))
              sym = paste("R_free_", id, "_finalizer", sep = "")  # see getDestructorNames in struct.R in RGCC..
            }

            tryCatch( f <- getNativeSymbolInfo(sym),
                      error = function(e) stop(simpleError(paste("the default finalizer symbol", sym, "is not available. Has the DLL been loaded"))))

#            .Call("R_RCppReference_addFinalizer", obj@ref, finalizer)
            addFinalizer(obj, f$address)
          })


setOldClass(c("CallRoutine", "NativeSymbolInfo"))
setOldClass("NativeSymbol")

setMethod("addFinalizer", c("RC++Reference", "NativeSymbolInfo"),
          function(obj, finalizer, default = character()) {
               #XXX want to check registration information.
             addFinalizer(obj, finalizer$address)
          })

setMethod("addFinalizer", c("RC++Reference", "NativeSymbol"),
          function(obj, finalizer, default = character()) {
             .Call("R_RCppReference_addFinalizer", obj@ref, finalizer)
          })


setMethod("addFinalizer", c("RC++Reference", "function"),
          function(obj, finalizer, default = character()) {
             .Call("R_RCppReference_addFinalizer", obj@ref, finalizer)
          })
