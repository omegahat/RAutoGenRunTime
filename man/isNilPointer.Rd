\name{isNilPointer}
\alias{isNilPointer}
\title{Query if an external pointer or RCReference is NULL}
\description{
  This function allows the caller to determine if the
  external pointer does not point to a valid address
  in memory but instead points to 0 or NULL.
  This is an R equivalent to checking if the external pointer
  is NULL at the C level
}
\usage{
isNilPointer(ref)
}
\arguments{
  \item{ref}{an external pointer or object that is derived from
    RCReference (or has a 'ref' slot that is an external pointer).}
}
\value{
  A logical value that is \code{TRUE} if the external pointer value is 0/NULL.
}

\author{
Duncan Temple Lang
}

\examples{
  p = new("externalptr")
  isNilPointer(p)

  r = new("RCReference", ref = p)
  isNilPointer(p)
}
\keyword{programming}
\keyword{interface}

