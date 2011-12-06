collectOutResults =
  #
  #  This is now done in C code via the generation mechanism.
  #
  #
  # called when we have out (or possibly inout) arguments
  # to gather the actual return value and the out arguments
  # from the call.
  # This takes care of copying the reference arguments to R values
  # or dropping them from the result or leaving them as references
  # depending on whether the corresponding element of .copy
  # is TRUE, NA or FALSE respectively.
function(ans, ...,
         .copy = structure(rep(FALSE, length(.refs)), names = names(.refs)),
         .refs = list(...))
{

    # discard the entries for which .copy is NA.
  if(any(is.na(.copy))) {
    i = match(names(.copy)[is.na(.copy)], names(.refs))
    .refs = .refs[ - i]
    .copy = .copy[!is.na(.copy)]
  }

  if(any(.copy))  {
    w = names(.copy)[.copy]
    .refs[w] = lapply(.refs[w], copyToR)
  }

  if(!missing(ans))
    c(.result = ans, .refs)
  else
    .refs
}

