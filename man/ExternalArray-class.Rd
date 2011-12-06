\name{ExternalArray-class}
\docType{class}
\alias{ExternalArray-class}
\alias{ExternalArrayWithLength-class}
\alias{ExternalMultiDimensionalArray-class}
\alias{ExternalPrimitiveTypeArrayWithLength-class}
\alias{ExternalTwoDimensionalArray-class}

\alias{[,ExternalArrayWithLength,missing,ANY-method}
\alias{[,ExternalArrayWithLength,numeric,ANY-method}
\alias{[<-,ExternalArrayWithLength,numeric-method}
\alias{[[,ExternalArrayWithLength,numeric,ANY-method}
\alias{[[<-,ExternalArrayWithLength,numeric-method}
\alias{[[<-,ExternalArrayWithLength,numeric-method}
\alias{dim,ExternalArrayWithLength-method}
\alias{duplicate,ExternalArrayWithLength-method}
\alias{length,ExternalArrayWithLength-method}
\alias{lapply,ExternalArrayWithLength-method}

\alias{[,ExternalTwoDimensionalArray,missing,missing-method}
\alias{[,ExternalTwoDimensionalArray,numeric,numeric-method}
\alias{[<-,ExternalTwoDimensionalArray,numeric-method}
\alias{[[<-,ExternalTwoDimensionalArray,numeric-method}
\alias{dim,ExternalTwoDimensionalArray-method}

\alias{dim,ExternalMultiDimensionalArray-method}

\alias{coerce,ExternalMultiDimensionalArray,array-method}
\alias{coerce,ExternalTwoDimensionalArray,array-method}





\title{Classes for representing referencs to native C/C++ arrays}
\description{
  These classes and methods provide the foundation for working
  with native arrays in C/C++ code as if they were vectors, matrices
  and arrays in R.
  The idea is that we have the address of or a reference to the
  native object and we have information about the dimensions
  of that object. We then create an R object that has that
  information and with this, we can access individual elements
  or collections of elements, and assign values from R to
  elements of the native array.

  Instances of these classes can be used without knowing the dimensions
  of the native arrays. In this case, the R programmer/user has to
  ensure that the indices are appropriate. Errors in this can cause R to crash.

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ExternalArray", ...)}.
Typically, such objects will be created in programmatically generated
code that provides an interface between R and existing native C/C++
code.
Sometimes, we will have an external pointer object in R
that we know refers to a native array and then we can
create an instance of \code{ExternalArray} in R using that reference.
But this is less robust as the user must ensure that the dimensions are
correct. This should be computed from the native code.
}
\section{Slots}{
	 \describe{
    \item{\code{elementSize}:}{Object of class \code{"integer"} the size
    of each element in bytes}
    \item{\code{ref}:}{Object of class \code{"externalptr"} the address
      of the native object }
    \item{\code{classes}:}{Object of class \code{"character"} a
      collection of class names for this object, typically corresponding
    to the native type.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{RC++Reference}"}, directly.
}

\section{Methods}{
No methods defined with class "ExternalArray" in the signature.
}

\references{}
\author{Duncan Temple Lang}

\seealso{
}
\examples{
showClass("ExternalArray")
}
\keyword{classes}
