TestFunctionPointers = TRUE

asFunctionPointer =
  #
  # If this is a function, where do we get the pointer at the end.
  # So this looks incomplete.
  #
  #
function(obj, numParams, paramTypes = NULL, test = TestFunctionPointers)
{
  if(is.function(obj)) {
     if(test && !inherits(obj, "AsIs")) {
       parms = formals(obj)
       is.missing = function(x) is.name(x) && as.character(x) == ""
       if(length(parms) < numParams && !("..." %in% names(parms)))
         stop("too few parameters in R function definition, expecting ", numParams)
       else if(sum(sapply(parms, is.missing)) > numParams)
         warning("potentially too many parmeters in R function")

          # Use the TypeInfo package's information for the function if it was set.
          # or from an S4 generic's methods
       funName = attr(obj, "generic")

            #XXX What is n here. Changed to numParams.
       if(!is.null(funName) && (length(paramTypes) || numParams == 0) && isGeneric(funName)) {
         if(is.null(selectMethod(funName,  paramTypes, optional = TRUE)))
           warning("there appears to be no method for the generic function ", sQuote(funName), " that matches the signature ", paste(sQuote(paramTypes), collapse = ", "))
       }
     }

     return(obj)
  }
  
  if(is.character(obj))
    obj = getNativeSymbolInfo(obj)
  
  if(inherits(obj, 'NativeSymbolInfo')) {
      if("numParameters" %in% names(obj) && obj$numParameters != numParams)
        stop("wrong number of parameters in routine")

      obj = obj$address
  }

  obj
}
