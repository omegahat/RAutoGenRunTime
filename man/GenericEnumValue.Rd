\name{GenericEnumValue}
\alias{GenericEnumValue}
\title{Create an enumerated value for an anonymous enumeration definition}
\description{
  When an enumeration is defined in with no name, such as
  \code{enum {A, B, C}}, then we do not generate a
  class in R corresponding to this type.
  Instead, we just create the R variables
  corresponding to the individual enmeration
  elements/symbols and we identify them
  as being of class \code{\link{EnumValue-class}}.
  This function creates such an instance.

  It can also be used to create an instance of
  a particular class of \code{\link{EnumValue-class}}.
}
\usage{
GenericEnumValue(name, val, class = "EnumValue", S3 = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{the symbolic name for the constant}
  \item{val}{the numeric value for the constant}
  \item{class}{the specific class for the enumeration type and collection.}
  \item{S3}{currently ignored. Related to using S3 classes.}
}
\value{
  An object of a class derived from \code{class}, typically
  specific to the enumeration type.
}
%\references{}
\author{Duncan Temple Lang}
\seealso{}
%\examples{}
\keyword{programming}
\keyword{interface}
