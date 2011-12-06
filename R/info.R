
getInterfaceInfo =
function(type)
{
  if(is(type, 'RC++Reference'))
    type = class(type)
  else if(!is.character(type))
    stop("an object or the name of a type")

  var = paste(".", type, ".InterfaceInfo", sep = "")
  if(!exists(var))
    stop("cannot find ", var, " which should have been generated with the interface")

  get(var)
}  

getInterfaceMethods =
function(type, access = c("public", "protected"))
{
  info = getInterfaceInfo(type)
  w = info$methods %in% access
  names(info$methods)[w]
}

getInterfaceFields =
function(type, access = c("public", "protected"))
{
  info = getInterfaceInfo(type)
  w = info$fields$access %in% access
  info$fields$names[w]
}
