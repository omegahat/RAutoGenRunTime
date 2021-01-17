\name{floatPtr-class}
\docType{class}
\alias{floatPtr-class}
\alias{intPtr-class}
\alias{voidPtr-class}
\alias{doublePtr-class}
\alias{longPtr-class}
\alias{shortPtr-class}
\alias{charRefRef-class}

\alias{[,doublePtr,ANY,ANY-method}
\alias{[<-,doublePtr,ANY,ANY-method}
\alias{[<-,doublePtr,ANY-method}

\alias{coerce,externalptr,voidPtr-method}
\alias{coerce,externalptr,character-method}
\alias{coerce,externalptr,doublePtr-method}
\alias{coerce,externalptr,floatPtr-method}
\alias{coerce,externalptr,intPtr-method}
\alias{coerce,RCReference,character-method}

\title{Representations of references to fundamental C/C++ data types}
\description{
 These classes provide references to C/C++ data types such as int,
 float, double, void and so on.
}
\section{Objects from the Class}{
}
\section{Slots}{
	 \describe{
    \item{\code{ref}:}{Object of class \code{"externalptr"} See
      RNativeReference class}
    \item{\code{classes}:}{Object of class \code{"character"} See
      RNativeReference class}
  }
}
\section{Extends}{
Class \code{"\linkS4class{RC++Reference}"}, directly.
}
\section{Methods}{
No methods defined with class "floatPtr" in the signature.
}
\references{}
\author{Duncan Temple Lang}


\seealso{

}
\examples{
showClass("floatPtr")
}
\keyword{classes}
