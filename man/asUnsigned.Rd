\name{asUnsigned}
\alias{asUnsigned}
%XXX Move
\alias{size_t-class}
\alias{coerce,integer,size_t-method}
\alias{coerce,numeric,size_t-method}


\title{Convert a vector to unsigned (positive) values}
\description{
  This function is used as a check on numeric values
  (either numeric or integer vectors)
  to ensure that the values are all non-negative.
  This is used when we pass values from R
  to a native routine that expects unsigned integers
  or size_t, etc. types.
  This raises an error or a warning if it finds any element
  that is negative.
}
\usage{
asUnsigned(value, type, force = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{the numeric values}
  \item{type}{the name of the class to which to coerce the \code{val}
    object. This is done first to provide an opportunity to "correct"
    any values.
  }
  \item{force}{a logical value. If \code{FALSE} and any of the elements
    of \code{val} are negative, the function raises an error.
    If \code{force} is \code{TRUE}, we just raise a warning.}
}
\value{
  The result of coercing \code{val} to class specified by \code{type}.
  If any values are negative, the function raises either a warning or an error.
}
\author{
Duncan Temple Lang
}
\seealso{
  \code{\link[methods]{coerce}}
The RGCCTranslationUnit package
}
\examples{
  x = c(0, 1, 2, 0, 1, 2)
  asUnsigned(x, "integer")

  asUnsigned(c(-1, x), "integer", force = TRUE)
  try(asUnsigned(c(-1, x), "integer", force = FALSE))

  setClass("truncatedUnsignedInt", contains = "integer")
  setValidity("truncatedUnsignedInt",
                function(obj)
                  if(!all(obj >= 0))
                      "some values are negative"
                  else
                     TRUE)

  setAs("numeric", "truncatedUnsignedInt",
            function(from) {
               if(any(from < 0))
                   from[from < 0 ] = 0L
               new("truncatedUnsignedInt", from)
            })
              
  asUnsigned(c(-1, x), "truncatedUnsignedInt", force = TRUE)
}
\keyword{programming}
\keyword{interface}
