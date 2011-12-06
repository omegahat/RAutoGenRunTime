\name{Free}
\alias{Free}
\title{Explicitly garbage collect a native object}
\description{
  This function allows the R programmer to explicitly
  release a native C/C++ object and free the memory associate with it.
  This is used when the usual garbage collection will not work
  by adding a finalizer when the R reference is created.
}
\usage{
Free(obj, recursive = FALSE, ...)
}
\arguments{
  \item{obj}{the reference to the native object that is to be released}
  \item{recursive}{a logical value indicating whether to free any fields
         within the object that are also references.}
  \item{\dots}{potential additional parameters for use by methods}
}
\value{
 \code{NULL}
}

\author{Duncan Temple Lang}
\seealso{
 generated constructors by RGCCTranslationUnit
}
\examples{

}
\keyword{programming}
