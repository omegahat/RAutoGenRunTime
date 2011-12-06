setClass("CNULLValue", contains = "integer", prototype = -919191L)
CNULLValue = new("CNULLValue")
setAs("CNULLValue", "character",
        function(from) {
             "CNULLValue"
        })



isNilPointer =
function(ref)
  .Call("R_isNilPointer", ref)
