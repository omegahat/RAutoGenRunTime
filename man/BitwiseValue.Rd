\name{BitwiseValue}
%\alias{SymbolicConstant-class}
%\alias{show,SymbolicConstant-method}
\alias{BitwiseValue}
%\alias{EnumerationValue}
%\alias{asEnumValue}
\alias{asBitwiseValue}

\alias{BitwiseValue-class}
%\alias{EnumerationValue-class}
%\alias{EnumValue-class}
%\alias{EnumDef-class}
%\alias{show,EnumDef-method}
\alias{[,EnumDef,ANY,ANY-method}

\alias{&}
\alias{|}
\alias{c}
\alias{&,BitwiseValue,BitwiseValue-method}
\alias{|,BitwiseValue,BitwiseValue-method}
\alias{c,BitwiseValue-method}
\alias{coerce,BitwiseValue,numeric-method}

\title{Create enumeration value}
\description{
 These functions and related classes and methods are used to work 
}
\usage{
BitwiseValue(val, name = names(val),
               class = if (is(val, "BitwiseValue"))
                         class(val)
                       else "BitwiseValue",
             asVector = TRUE, S3 = FALSE,
             defValues = getEnumValues(class, name))
}
\arguments{
  \item{val}{the value which is to be converted to the appropriate
    enumerated class. This can be a numeric value or vector, or a
    character vector giving the names of the symbolic constants within
    the enumeration set.}
  \item{name}{the symbolic name(s) of the value(s) in \code{val}. This
    is the symbolic names from the enumeration set.}
  \item{class}{the name of the R class corresponding to the specific
    enumeration type. }
  \item{asVector}{a logical value controlling the case where there is
    more than one value in \code{val} and how we return it.
    We can process each value separately, or we can process them as a
    collection of values to be OR'ed together.
    If this is \code{TRUE}, then we OR the values together.
    Otherwise, we treat each value separtely and reutrn a list of these
    processed values.
  }
  \item{S3}{currently ignored. a logical value. This is no longer used
    as we use S4 classes for these enumeration types}
  \item{defValues}{the named vector  of symbolic name and value pairs
    that define the enumeration collection. We try to find this
    based on the name of the class of \code{val} or \code{class}
    or from the names of the variables
    in \code{val} if this is a string.
   }
}
\value{
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{
}
\examples{
 # See tests/bits.R
}
\keyword{programming}
\keyword{interface}

