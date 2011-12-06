\name{SymbolicConstant-class}
\Rdversion{1.1}
\docType{class}
\alias{SymbolicConstant-class}
\alias{EnumerationValue-class}
\alias{show,SymbolicConstant-method}

\title{Class "SymbolicConstant"}
\description{
  This class and its derived classes are used for representing
  symbolic constants, and specifically collections of related symbolic
  constants.  The most common are enumeration values which in C
  are integers that are identified by more meaningful symbolic
  names, e.g. RED, GREEN, BLUE; HTTP, HTTPS, FTP; SQUARE, CIRCLE, ...
  The idea is that possible values come from a finite set, much like
  the levels of a factor.   The enumeration type specifies
  that set and gives symbolic names to the possible values.

  An additional type of symbolic constant is one where the elements
  identify a possible setting but that these settings are not mutually
  exclusive.  Different combinations of settings can be specified by
  combining the different possible values. These are
  bitwise-enumerations.

  The idea behind the SymbolicConstant class, the EnumerationValue
  class and the BitwiseValue class is to represent single values
  for these different types.
  A SymbolicConstant is an integer.
  An EnumerationValue is derived from this and is associated with a set
  of possible values for that enumeration.
  A BitwiseValue is also derived from SymbolicConstant and
  is associated with a set of possible settings. These can be combined
  and the package provides operators to do this.
}
\section{Objects from the Class}{
  We typically use EnumDef, EnumValue and
  define methods for coercing numbers and strings to
  the corresponding EnumerationValue or BitwiseValue objects.
  In this regard, code is typically machine generated from the
  simple description of \code{name = value} pairs that define
  the enumeration or bitwise-enumeration.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"integer"} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{integer}"}, from data part.
Class \code{"\linkS4class{numeric}"}, by class "integer", distance 2.
Class \code{"\linkS4class{vector}"}, by class "integer", distance 2.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "integer", distance 2.
Class \code{"\linkS4class{EnumerationValue}"}, by class "numeric", distance 3.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SymbolicConstant")}:
        this displays the integer value along with the symbolic name and
         also the }
	 }
}
\references{
  C programming books, e.g. Kernighan & Ritchie
}
\author{
Duncan Temple Lang
}


\seealso{

}
\examples{
showClass("SymbolicConstant")
}
\keyword{classes}
\keyword{programming}

