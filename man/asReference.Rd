\name{asReference}
\alias{asReference}
\alias{asReference,VariableReference-method}
\alias{asReference,ANY-method}
\alias{asReference,NULL-method}

\alias{coerce,VariableReference,logical-method}
\alias{coerce,VariableReference,numeric-method}
\alias{coerce,VariableReference,integer-method}
\alias{coerce,VariableReference,ANY-method}

\alias{coerce,CNULLValue,character-method}

\title{Create a reference object to a native compiled object}
\description{
  This function and its methods are used to create a
  reference to an external, native, compiled object.
}
\usage{
asReference(x, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the object which we will reference. This should be an
    external pointer object}
  \item{type}{the type of the reference. This is used as the class of
     the new object. This is the name of an existing R class.}
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
}
\keyword{programming}
\keyword{interface}
