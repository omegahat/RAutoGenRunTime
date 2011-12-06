\name{asFunctionPointer}
\alias{asFunctionPointer}
\title{Convert an object to a pointer to a native routine}
\description{
  (This looks incomplete.)
  
  This function is used to coerce a value to a reference to a
  native routine.
  We also have methods for \code{\link[methods]{coerce}}
  to do this for strings and NativeSymbolInfo and NativeSymbol
  objects.
  However, this function is capable of doing more than simple
  coercion but testing whether the object is compatible with
  the native routine.  That means we test the number of parameters
  and their types.
}
\usage{
asFunctionPointer(obj, numParams, paramTypes = NULL, test = TestFunctionPointers)
}
\arguments{
  \item{obj}{the object to be converted.}
  \item{numParams}{the number of parameters the routine will be passed}
  \item{paramTypes}{a list giving the types of the parameters. If the
     we have information about the C routine or about the types of the R function's
     parameters, we can check that these are compatible with the
     expected function pointer types.
     Note that we don't check the return type here!}
  \item{test}{a logical value which controls whether we test the number
    and types of parameters for R functions passed as \code{obj}.
    If the \code{obj} is of class "AsIs", i.e. given as \code{I(obj)},
    then this argument is ignored.
    }
}
\details{
}
\value{
 \code{obj}, or an error is raised.
}
\references{
}
\author{
Duncan Temple Lang
}

\seealso{
 \code{\link{coerce,character,RNativeRoutineReference-method}}
}
\examples{

}
\keyword{interface}
\keyword{programming}
