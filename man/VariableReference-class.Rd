\name{VariableReference-class}
\Rdversion{1.1}
\docType{class}
\alias{VariableReference-class}
%\alias{asReference,VariableReference-method}
%\alias{coerce,VariableReference,ANY-method}
%\alias{coerce,VariableReference,integer-method}
%\alias{coerce,VariableReference,logical-method}
%\alias{coerce,VariableReference,numeric-method}

\title{Class "VariableReference"}
\description{
  This class allows us to represent a reference to a native variable.
  We hold the address of the native variable in our R object
  and can access the value of it from within R via this object.
  We do not hold the value of the variable in the R object
  but the address of the native variable. This allows us
  to query its value at any moment, and allow us to
  obtain its value should it change.
  In some ways, this acts like a dynamic variable.
  We can get the value of the native variable at any time
  but the value assigned to that variable may change at any time
  (due to native code).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("VariableReference", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{ref}:}{Object of class \code{"RC++Reference"}
    This is the address of/reference to the native object.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ANY}"}, directly, with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{asReference}{\code{signature(x = "VariableReference")}: ... }
    \item{coerce}{\code{signature(from = "VariableReference", to =
	"ANY")}: coerce the value of the native variable to an R object }
    \item{coerce}{\code{signature(from = "VariableReference", to =
	"integer")}: coerce the value of the native variable to an R integer vector}
    \item{coerce}{\code{signature(from = "VariableReference", to =
	"logical")}: coerce the value of the native variable to an R
      logical vector }
    \item{coerce}{\code{signature(from = "VariableReference", to =
	"numeric")}: coerce the value of the native variable to an R
      numeric vector}
	 }
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{
The \code{RGCCTranslationUnit} package
}
\examples{
showClass("VariableReference")
}
\keyword{classes}
