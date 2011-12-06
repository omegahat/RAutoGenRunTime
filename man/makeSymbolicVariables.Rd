\name{makeSymbolicVariables}
\alias{makeSymbolicVariables}
\title{Define R variables for native enumeration constants}
\description{
  This function is used to define R variables that correspond
  to the values of enumeration constants in C/C++.
}
\usage{
makeSymbolicVariables(def, className = class(def), where = globalenv())
}
\arguments{
  \item{def}{ a named vector of values. The names are used as the names
    of the R variables to be defined. The values are the values used for
    the values of the variables.}
  \item{className}{the name of the R class for each element being
    defined.  This allows us to define classes for specific groups of
    enumerated values  rather than using a generic class.
   This is currently only used for bitwise enumerations.}
  \item{where}{the environment in which to define the new R variables.}
}

\value{
 The function is used for its side effects of defining new variables.
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
  vals = c(Red = 1L, Green = 2L, Blue = 3L)
  makeSymbolicVariables(vals)
}
\keyword{programming}
\keyword{interface}
