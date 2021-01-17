\name{RC++Reference-class}
\docType{class}
\alias{RNativeReference-class}
\alias{NativeSymbol-class}
\alias{RNativeRoutineReference-class}
\alias{RCReference-class}
\alias{RC++Reference-class}
\alias{RC++ReferenceUseName-class}
\alias{RC++StructReference-class}
\alias{RCStructReference-class}
\alias{addFinalizer,RC++Reference,logical-method}
\alias{addFinalizer,RC++Reference,NativeSymbolInfo-method}
\alias{addFinalizer,RC++Reference,NativeSymbol-method}
\alias{addFinalizer,RC++Reference,function-method}
\alias{addFinalizer,RC++Reference,character-method}

\alias{[,RCStructReference,missing,missing-method}
\alias{[,RCStructReference,character,missing-method}
\alias{[[,RCStructReference,character,missing-method}

\alias{$,RC++Reference-method}
\alias{$,RC++ReferenceUseName-method}
\alias{Free,RC++Reference-method}

\alias{coerce,character,RNativeRoutineReference-method}
\alias{coerce,NativeSymbolInfo,RNativeRoutineReference-method}
\alias{coerce,NativeSymbol,RNativeRoutineReference-method}
\alias{coerce,externalptr,RNativeRoutineReference-method}

\title{General classes for a reference to a C++ object}
\description{ The class \code{RC++Reference} is the class that holds the address of a C++ object.
  With the programmatically constructed code (or manually)
  we can then invoke its C++ methods and access its public fields from within R and generally
  manipulate the C++ object.

  The class \code{RC++ReferenceUseName} class is a simple extension of
  \code{RC++Reference} which exploits the fact that the R
  functions corresponding to the C++ methods for an instance are of the
  form \code{class_<method name>}, e.g. MyClass_plot for a  reference
  class named \code{MyClassPtr}.
}

     
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RC++Reference", ...)}.
They are usuall returned from calls to R functions that call C++ code
to create the object.
}
\section{Slots}{
	 \describe{
    \item{\code{ref}:}{Object of class \code{"externalptr"} this is the
      R object that holds the address of the C++ object.}
    \item{\code{classes}:}{Object of class \code{"character"} the name
      of R classes that correspond to the C++ class hierarchy.}
  }
}
\section{Methods}{
  \describe{
    \item{addFinalizer}{\code{signature(obj = "RC++Reference", finalizer = "logical")}:  }
    \item{addFinalizer}{\code{signature(obj = "RC++Reference", finalizer = "NativeSymbolInfo")}:  }
    \item{addFinalizer}{\code{signature(obj = "RC++Reference", finalizer = "NativeSymbol")}:  }
    \item{addFinalizer}{\code{signature(obj = "RC++Reference", finalizer = "function")}:  }
    \item{$}{\code{signature(x = "RC++Reference")}: ... }
    \item{Free}{\code{signature(obj = "RC++Reference")}: ... }
	 }
}
\references{
 \dQuote{Writing R Extensions Manual}, R Development Core Team
  
 The RAutoGenRunTime package for programmatically reading C/C++ code and
 generating bindings for the data structures, methods and routines.
}
\author{Duncan Temple Lang}

\seealso{
}
\examples{
showClass("RC++Reference")
}
\keyword{classes}
