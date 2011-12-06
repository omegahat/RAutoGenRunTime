\name{CNULLValue}
\alias{CNULLValue}
\alias{CNULLValue-class}
%\docType{data}
\title{Represent the \code{NULL} value in C.}
\description{
  This variable represents \code{NULL} in C and
  provides a similar role as \code{NULL} in R,
  i.e. allowing us to compare an R object that is a
  reference to a native object to the C value \code{NULL}.
  We can also do this with \code{\link{isNilPointer}} which
  is a better approach.
}
\usage{CNULLValue}
\format{
This is represented as an integer.
}
\details{
}
\references{
}
\examples{
data(CNULLValue)
}
\keyword{datasets}
