\name{duplicate}
\alias{duplicate}
\title{Create a copy of a native object}
\description{
  This is a generic function that allows an R programmer
  to create a duplicate copy of a native C/C++-level object.
}
\usage{
duplicate(x, ..., .finalizer = FALSE)
}

\arguments{
  \item{x}{the object to be cloned}
  \item{\dots}{additional parameters for methods}
  \item{.finalizer}{a value controlling the registration of a C-level
    finalizer. This can be a logical value, a native symbol object
    (\code{NativeSymbolInfo}) or a string giving the name of a native symbol.
     }
   }
\details{
  When an interface to a C/C++ data structure is created, a copy routine
  should have been created and a method for the R reference class
  defined for this method.
}   
\value{

}
\author{ Duncan Temple Lang}

\seealso{
  The RGCCTranslationUnit package.

  Garbage collection and Writing R Extensions Manual for
  information about finalizers.
}
%\examples{}
\keyword{programming}

