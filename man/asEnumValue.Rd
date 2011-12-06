\name{asEnumValue}
\alias{asEnumValue}
\title{Validate and convert a name or value to an enumeration constant}
\description{
  This allows the user to specify a name or value
  when an enumerated constant is expected
  and this function ensures that it is valid.  The function returns a formal
  object of that enumeration class containing both the
  name and value which can be passed to C code that
  expects an object of that type.

  This is typically not called directly by the R user
  but implicitly by generated code that coerces
  user input to a  particular type of
  enumerated constant.  Such code is typically
  programmatically generated and uses the expression
  \code{as(val, "NameOfEnumType")}, with the appropriate
  name for the type of enumeration.
  
  This function also attempts to fix typos by using
  \code{\link[base]{agrep}} to find close matches.
  It can raise either an error or a warning
  with information about the possible matches
  and can continue on using them if \code{fixCloseMatches} is
  \code{TRUE}.
}
\usage{
asEnumValue(val, values, class = values@EnumName, fromString = NA,
            fixCloseMatches = TRUE, prefix = character(),
               S3 = is.null(getClassDef(class)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{val}{either the symbolic name or number which is to be validated
  and coerced to an EnumValue of the particular \code{class}.}
\item{values}{the \code{EnumDef} object that contains both
  the name-value pairs of possible values as a vector, and also the
  class of the associate instances. This is a derived class from
  \code{\link{EnumValue-class}}.}

 \item{class}{the class of the object that the function will return, i.e.
   the type of enumeration.  The default is almost
   always the correct value for enumerations.
   For BitwiseValues, the target class is usually specified.
  }
  \item{fromString}{indicates whether the the value is being specified
    as a name or a number. This is rarely needed and is determined by
    the function.  But it does allow us to treat a string
   as a number.}
 \item{fixCloseMatches}{a logical value. 
   For any elements of \code{val} which is not matched in
   \code{values} (either by value or name),
   we use \code{\link[base]{agrep}} to find "close" matches.
   If this argument is  \code{TRUE},
   then we use those matches and proceed (although raise a warning which
   identifies the erroneously specified values and the "correct" ones
   so that the user can correct the call).
   If this is \code{FALSE}, we raise an exception
   and the exception object contains the suggested replacments.
 }
 \item{prefix}{a prefix string that is common to all of the symbolic
   names.
   This allows us to use this as a prefix for the name if it is not an
   exact match to the defined names for the collection.  In other words,
   we can allow the caller to use a shorthand form without the common
   prefix, if there is one.}
 \item{S3}{ignored. Related to using S3 classes and unlikely to be used.}
}
\value{
  An object class \code{class}
  which is typically obtained from the
  \code{EnumName} slot in the \code{values}
  argument defining the possible enumerated values.
}
\references{}
\author{Duncan Temple Lang}
\seealso{
  The \code{RGCCTranslationUnit} package at
  \url{http://www.omegahat.org/RGCCTranslationUnit}.
}
\examples{

edef = EnumDef("Color", c("red" = 1, "blue" = 2, "green" = 3,
                          "yellow" = 4, "grey" = 10))
x = asEnumValue("red", edef)
class(x)

y = asEnumValue(c("red", "yellow"), edef)

 # Illustrates when we specify a value slightly incorrectly.

  # go on but issue a warning.
asEnumValue(c("red", "gray"), edef, fixCloseMatches = TRUE)

    # raise an error.
 try(asEnumValue(c("red", "gray"), edef, fixCloseMatches = FALSE))

}
\keyword{programming}
\concept{enumeration}
\concept{symbolic constant}
\concept{C/C++}
