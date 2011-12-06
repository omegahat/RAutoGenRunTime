\name{fixArrayLength}
\alias{fixArrayLength}
\title{Set length of R object to length of target native array}
\description{
  This function is used (for better or worse)
  when coercing an R vector (including a list)
  to an appropriate form. It changes the length of the
  R object to match that of the array.
}
\usage{
fixArrayLength(obj, expectedLength, fatal = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ the R object whose length is to be changed}
  \item{expectedLength}{the length of the target native array}
  \item{fatal}{a logical value indicating whether to report an error or
    just a warning about the mismatched lengths}
}
\value{

}
\author{Duncan Temple Lang}
\seealso{}
\examples{

}
\keyword{programming}
