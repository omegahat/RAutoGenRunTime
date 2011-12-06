\name{isNativeNull}
\alias{isNativeNull}
\title{Test whether an external pointer has the C-level NULL value}
\description{
  This function allows us to determine from R whether the contents
  of an external pointer are NULL.
  See also \code{\link{isNilPointer}}. (Basically a repeat of this taken
  from another package.)
}
\usage{
isNativeNull(x, andNULL = TRUE)
}
\arguments{
  \item{x}{the external pointer object to be tested}
  \item{andNULL}{if \code{x} is \code{NULL} in R and \code{andNULL} is
    \code{TRUE},
    the function returns \code{TRUE} immediately. So this is only to
    handle the case where \code{x} is the R \code{NULL} value.}
}
\value{
A logical scalar value.
}
\author{Duncan Temple Lang}

\examples{
}
\keyword{programming}
