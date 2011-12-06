\name{copyToR}
\alias{copyToR}
\title{Copy a native C/C++ object to an R representation}
\description{
  This function is intended to be a simple coercion function that converts
  a reference to C/C++ object to the corresponding
  class in R.  Each '*Ptr' class has an associated R class that mirrors
  the fields in the C/C++ structure with slots in the R class
  definition.
  This function converts the '*Ptr' object to this R-based form.
  It is meant as a simplified way to perform coercion with
  \code{as(obj, "MyClass")} where \code{obj} is an object of class
  \code{MyClassPtr}.  This approach means the caller doesn't have to
  know   the name of the target R class (or of the reference class).
}
\usage{
copyToR(obj)
}
\arguments{
  \item{obj}{the reference to the native data structure that is to be
    copie to an R representation.}
}
\value{
}

\references{
 \dQuote{Writing R Extensions Manual}, R Development Core Team
  
 The RAutoGenRunTime package for programmatically reading C/C++ code and
 generating bindings for the data structures, methods and routines.
}
\author{Duncan Temple Lang}

\seealso{}
\examples{
}
\keyword{programming}
\keyword{interface}
