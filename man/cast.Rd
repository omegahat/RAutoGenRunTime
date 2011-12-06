\name{cast}
\alias{cast}
\alias{cast,ANY,character-method}
\title{Performs C++-style casting}
\description{
  This function provides access to the different styles of C++ coercion
  or casting.
}
\usage{
cast(from, to, how = `C++CastValues`["reinterpret"])
}
\arguments{
  \item{from}{the object to cast}
  \item{to}{the target type to which the object will be cast}
  \item{how}{the type of C++ casting, dynamic, reinterpret, static.}
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
