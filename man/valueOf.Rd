\name{valueOf}
\alias{valueOf}
\title{Get the contents of a reference object}
\description{
  This function (AFAIR) is for dereferencing or retrieving
  the contents of an external pointer.
  This is a generic equivalent of
  \code{as(ref, "RefererenceType")} where
  the caller has to know the name of \code{ReferenceType}.
  This is also equivalent in C to \code{*ref}.
}
\usage{
valueOf(obj)
}
\arguments{
  \item{obj}{the reference object}
}
\details{
  When (programmatically) generating an interface to native
  data structures,  a method should be provided for this
  generic fnction.
}
\value{
  An R object representing the contents  of the reference object.
}
  
\author{Duncan Temple Lang}
\seealso{
 RGCCTranslationUnit package
}
\examples{
}
\keyword{programming}

