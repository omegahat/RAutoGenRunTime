validateCopy =
  #
  # ensures that the specified value 'val'
  # for a .copy argument in a programmatically
  # generated R function has entries for all the 
  # required parameters and is a
  #
function(val, outArgs)
{
  if(length(names(val)) == 0 && length(outArgs) == length(val))
    names(val) = outArgs
 

  i = match(names(val), outArgs)
  if(any(is.na(i)))
    stop("entries in .copy are not names of out args: ", names(val)[is.na(i)])
  i = match(outArgs, names(val))
  if(any(is.na(i)))
    val[outArgs[is.na(i)]] <- FALSE
  
  val
}  
