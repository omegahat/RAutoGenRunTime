# From edefs[["wxStretch"]] where edefs are the enumerations for the TU
# for wxWidgets.


library(RAutoGenRunTime)
setClass("wxStretch", contains = "BitwiseValue")
wxStretchValues = BitwiseValue(structure(c(0L, 4096L, 8192L, 8192L, 16384L, 32768L, 49152L, 
1048576L), .Names = c("wxSTRETCH_NOT", "wxSHRINK", "wxGROW", 
"wxEXPAND", "wxSHAPED", "wxFIXED_MINSIZE", "wxTILE", "wxADJUST_MINSIZE"
)), class = "wxStretch")


setAs("character", "wxStretch",
       function(from) {
         asBitwiseValue(from, wxStretchValues, "wxStretch")
       })

setAs("numeric", "wxStretch",
       function(from) {
         asBitwiseValue(from, wxStretchValues, "wxStretch")
       })

setAs("integer", "wxStretch",
       function(from) {
         asBitwiseValue(from, wxStretchValues, "wxStretch")
       })




      

as("wxGROW", "wxStretch")
as(c("wxGROW", "wxTILE"), "wxStretch")

as(8192, "wxStretch")
as(c(8192, 4096), "wxStretch")
as(as.integer(c(8192, 4096)), "wxStretch")


           # make top-level variables
makeSymbolicVariables(wxStretchValues)

# now check things work.
a = wxGROW | wxEXPAND | wxTILE
class(a) == "wxStretch"
length(a) == 1
names(a) ==  "wxGROW | wxEXPAND | wxTILE"

tt = c(wxGROW, wxEXPAND, wxTILE)
class(tt) == "wxStretch"
length(tt) == 3

as(tt, "numeric") == a

names(as(tt, "numeric"))

 # straight to a number, but with the checks.
asBitwiseValue(tt, wxStretchValues, NA)


(wxGROW | wxSHRINK) & wxSHRINK

c(wxGROW, wxEXPAND, wxTILE) & wxTILE

c(wxGROW, wxSHRINK, wxGROW, wxTILE) & wxTILE


####################################################################################################

# Enums.
# See C++Cast also

v = structure(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 12L, 65535L
), .Names = c("wxZIP_METHOD_STORE", "wxZIP_METHOD_SHRINK", "wxZIP_METHOD_REDUCE1", 
"wxZIP_METHOD_REDUCE2", "wxZIP_METHOD_REDUCE3", "wxZIP_METHOD_REDUCE4", 
"wxZIP_METHOD_IMPLODE", "wxZIP_METHOD_TOKENIZE", "wxZIP_METHOD_DEFLATE", 
"wxZIP_METHOD_DEFLATE64", "wxZIP_METHOD_BZIP2", "wxZIP_METHOD_DEFAULT"
))
setClass("wxZipMethod", contains = "EnumValue")
wxZipMethodValues = EnumDef("wxZipMethod", v)
makeSymbolicVariables(wxZipMethodValues)

wxZipMethodValues[1]
wxZipMethodValues[1:3]

setAs('character', 'wxZipMethod',
       function(from)
         BitwiseValue(from, from, class = "wxZipMethod"))


as("wxZIP_METHOD_STORE", 'wxZipMethod')
as(c("wxZIP_METHOD_STORE", "wxZIP_METHOD_SHRINK"), 'wxZipMethod')
as(1, 'wxZipMethod')
as(c(1, 2), 'wxZipMethod')

# Error
tryCatch(as("wxZIP_METHOD_SHRINL", "wxZipMethod"), error = function(e, ...) cat("Intentional error\n"))
tryCatch(as("wxZIP_METHOD_SHRINL", "wxZipMethod"), EnumCoercionError = function(e, ...) cat("I bet you meant", paste(e$possibleValues, collapse = ", "), "\n"))

tryCatch(as(65534, "wxZipMethod"))
