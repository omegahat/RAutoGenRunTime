\name{addFinalizer}
\alias{addFinalizer}
\title{Register a cleanup finalizer for a C/C++ object}
\description{
  This function registers a native finalizer with the
  specified external pointer. This routine is then
  invoked when the object is garbage collected.
  This allows us to free resources and memory associated
  with this object.
}
\usage{
addFinalizer(obj, finalizer, default = character())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{the object to which the finalizer is to be associated.}
  \item{finalizer}{the routine that is to be called when the \code{obj}
    is garbage collected. This can be an R function, a character string giving the name
  of the routine to invoke, a NativeSymbolInfo object identifying the
  routine, or the address of the routine. See \code{getNativeSymbolInfo}}
  \item{default}{the finalizer object}
}

\value{
 A logical value indicating whether the finalizer was registered.
}
\references{
Writing R Extensions Manual.
}
\author{
Duncan Temple Lang
}

\seealso{

}
\examples{

}
\keyword{programming}

