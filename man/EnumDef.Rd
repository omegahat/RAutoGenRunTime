\name{EnumDef}
\alias{EnumDef}
\alias{EnumDef-class}
\alias{show,EnumDef-method}
\alias{[,EnumDef-method}
\alias{[,EnumDef,ANY-method}
\title{Define a collection of related constants}
\description{
  This function and associated class allow us to define
  an enumerated constant type in R that specifies
  the names and integer values for the symbolic
  constants.
}
\usage{
EnumDef(name, values, symbolicNames = names(values))
}
\arguments{
  \item{name}{name of the enumeration group }
  \item{values}{the values of the individual elements of the enumeration
    group. This is typically a named integer vector where the names
    are the symbolic names to be used.}
  \item{symbolicNames}{the symbolic names for the values. This parallels
  the \code{values} vector.}
}
\value{
}
\references{}
\author{Duncan Temple Lang}

\seealso{
%  \code{\link{EnumValue}}
  \code{\link{BitwiseValue}}
}
\examples{

}
\keyword{programming}

