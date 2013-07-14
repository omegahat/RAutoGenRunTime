setClass("intPtr", contains = "RC++Reference")
setClass("doublePtr", contains = "RC++Reference")
setClass("longPtr", contains = "RC++Reference")
setClass("shortPtr", contains = "RC++Reference")
setClass("floatPtr", contains = "RC++Reference")
setClass("voidPtr", contains = "RC++Reference")

setAs("externalptr", "voidPtr", function(from) new("voidPtr", ref = from))

#setClass("void", contains = "voidPtr") # can't have a void so  make void synonomous with voidPtr for simplicity

# setClass("intArray", contains = "ExternalPrimitiveTypeArrayWithLength")

setMethod("[", "doublePtr",
          function(x, i, j, ..., drop = TRUE) {
             index = c(i, if(!missing(j)) j, ...)
             
             .Call("R_getDoublePtr_els", x@ref, as.integer(index - 1))
          })

setMethod("[<-", "doublePtr",
          function(x, i, j, ..., value) {
             index = c(i, if(!missing(j)) j, ...)
             value = as.numeric(value)
             if(length(value) < length(index))
               value = rep(value, length = length(index))
             .Call("R_getDoublePtr_setEls", x, as.integer(index - 1), value)
          })

