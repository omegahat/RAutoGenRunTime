\name{validateCopy}
\alias{validateCopy}
\title{Validate an object for copying to a native structure}
\description{
  This is a run-time helper function that ensures that the specified value \code{val}
  for a .copy argument in a programmatically  generated R function has
  correct entries for all the required parameters.
  Basically, in the RGCCTranslationUnit package, we generate C routines
  and R functions that act as wrappers to existing C/C++ routines.
  Some of those existing routines have a parameter that is a pointer to
  a structure or array or return such a pointer.
  When we invoke those routines through our wrapper routines, we
  want to be able to have the either the pointer or its contents
  returned as an R object.
  To this end, our wrapper functions and routines add a a parameter
  (named  .copy in the R function) and this is a logical vector
  controlling
  which of these pointer parameters are to be copied to R values.
  If the value for an element is \code{FALSE}, then we return the
  pointer; if it is \code{TRUE}, we copy the pointer value to its R
  equivalent, e.g. an R vector or an S4 class.

  This function ensures that the value of the \code{.copy} argument
  in a call to the R wrapper function is of the correct length and form
  so that the C routines don't have to check this. So this function is
  made available at run-time for the programmatically-generated code to
  access. It is not expected to be used by R programmers directly.
}
\usage{
validateCopy(val, outArgs)
}
\arguments{
  \item{val}{the value of the \code{.copy} argument in the R wrapper function}
  \item{outArgs}{the names of the parameters that are mutable,
     i.e. which can be returned as a pointer or by value.}
}
\details{

}
\value{
 A logical vector
}
\author{
Duncan Temple Lang
}


\seealso{
The RGCCTranslationUnit package and the code to generate wrapper
functions and routines.
}
\examples{

}
\keyword{programming}
\keyword{interface}
