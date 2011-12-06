\name{checkMethodsNumArgs}
\alias{checkMethodsNumArgs}
\title{Check the number of arguments for a native method/routine}
\description{
  This function is used to verify that an R function being used
  as a proxy for a C/C++ routine or method has the
  approrpriate number of parameters so that calls to it from
  the intermediate (programmatically generated) C/C++ code
  will not fail because there are too many values being passed to it.
  So in short, this just ensures that the R function has enough
  parameters, including the \dots parameter.

  This is a vectorized function. It takes the number of arguments
  expected as a vector and then a collection of functions
  via the \dots parameter or via the funs

  See \code{\link{asFunctionPointer}}.
}
\usage{
checkMethodsNumArgs(expected, ..., .funs = list(...))
}
\arguments{
  \item{expected}{an vector giving the number of parameters for the
    methods of interest. This can be a named vector in which case the
    names are matched with those of \code{.funs}. }
  \item{\dots}{one or more R functions. These can be named arguments and
  the names are used to find the expected number of parameters in \code{expected}.}
  \item{.funs}{an alternative way to specify the R functions which is
    convenient if they are already in a list.}
}
\value{
}
\references{
}
\author{
Duncan Temple Lang
}

\note{See asFunctionPointer. The code could be shared by these two.}

\seealso{

}
\examples{
  a = function(x, y) { }
  b = function(x, y, ...) { }

  checkMethodsNumArgs(c(2, 4), a, b)

  fail = function(x, y) { }
  try(checkMethodsNumArgs(c(2, 4), a, fail))

  try(checkMethodsNumArgs(c(b = 2, f = 4), b = b, f = fail))
}
\keyword{programming}
\keyword{interface}

