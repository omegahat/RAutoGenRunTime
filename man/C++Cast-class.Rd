\name{C++Cast-class}
\docType{class}
\alias{C++Cast-class}
\alias{coerce,character,C++Cast-method}
\alias{coerce,numeric,C++Cast-method}
\alias{C++CastValues}


\title{Enumeration for types of C++ casting/coercion}
\description{
  The \code{C++Cast} and \code{C++CastValues} provide the definition
  for a collection of related enumerated constants that allow the
  R programmer to identify different types of casting of
  C++ references to another class.
  There are four types of casting in C++ as opposed to just one in C.
  These are static, dynamic, reinterpret and const.
  These have different characteristics including run-time coercion
  rather than compile-time knowledge.

  This class and \code{EnumDef} prvide the definitions so that we can
  use either the names or numerical values when idenifying a type of cast.
}
\section{Objects from the Class}{

}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"integer"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{EnumValue}"}, directly.
Class \code{"\linkS4class{SymbolicConstant}"}, by class "EnumValue", distance 2.
Class \code{"\linkS4class{EnumerationValue}"}, by class "EnumValue", distance 2.
Class \code{"\linkS4class{integer}"}, by class "EnumValue", distance 3.
Class \code{"\linkS4class{vector}"}, by class "EnumValue", distance 4.
Class \code{"\linkS4class{numeric}"}, by class "EnumValue", distance 4.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "EnumValue", distance 4.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "character", to = "C++Cast")}: }
    \item{coerce}{\code{signature(from = "numeric", to = "C++Cast")}:  }
	 }
 }
\references{C++ Programmers Reference}
\author{Duncan Temple Lang}

\seealso{
}
\examples{
showClass("C++Cast")

as(1, "C++Cast")
as("static", "C++Cast")
}
\keyword{classes}
