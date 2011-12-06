\name{bitlist}
\alias{bitlist}
\alias{cumBitOr}
\title{Compute the bitwise OR of one or more values and/or vectors}
\description{
  This function is used to compute the bitwise OR
  of a collection of values given in a vector
  or individually via the \dots mechanism
  (or both).  It is equivalent to the C code
  \code{a | b | c}
  and that form is also supported for
  \code{BitwiseValue} objects.
  And this function uses
  \code{bitlist(a, b, c)}
  or
  \code{bitlist(c(a, b, c))}.
  These two forms support interactive use
  and programmatic use where the
  values are already in a single container (i.e. vector or list).

  This uses Steve Dutky's bitops package.
}
\usage{
bitlist(...)
}
\arguments{
  \item{\dots}{individual values or vectors which are
    all unravelled into a single vector
    via \code{\link[base]{unlist}}
    }
}
\value{
  A single number which is the bitwise OR'ing
  of all the elements performed successively.

}
\references{
  Steve Dutky's \code{bitops} package
  and specifically \code{\link[bitops]{bitOr}}
  package.
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{BitwiseValue-class}}
}
\examples{
 bitlist(c(2, 3, 8))
 bitlist(2, 3, 8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{programming}
\concept{bitwise OR}

