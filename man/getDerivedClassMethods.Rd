\name{getDerivedClassMethods}
\alias{getDerivedClassMethods}
\alias{setDerivedClassMethods}
\alias{makeDerivedMethodsList}

\alias{setDerivedClassMethods,RDerivedClass-method}
\alias{setDerivedClassMethods,character-method}
\alias{getDerivedClassMethods,character-method}


\title{Get and the methods for a derived classes implemented via R objects}
\description{
  A C++ class has methods and fields.
  We can create an extension of such a class which has methods that are
  implemented as R functions.
  Basically, we define a new C++ class that extends the original C++
  class.
  For each method in the new C++ class, we implement it
  by having it check to see if there is an R function registered for
  that   method. If so, the C++ code invokes that R function, passing
  the arguments from the C++ call to the R function, and returning the R
  value in the appropriate form.
  If there is no R function registered for that method, the C++ code
  calls the inherited method.
  In this way, we can use an R implemented version of any C++ class
  without modifying any existing compiled code.

  These functions and methods allow us to access and register
  R functions registered for  different methods of the C++ class.
}
\usage{
getDerivedClassMethods(obj, mergeClassMethods = TRUE, simplify = FALSE)
}
\arguments{
  \item{obj}{either the name of the R class or an instance of that class}
  \item{mergeClassMethods}{a logical value that }
  \item{simplify}{a logical value}
}
\value{

}
\author{
Duncan Temple Lang
}

\seealso{
  The RGCCTranslationUnit package and the code it
  generates to interface to C++ classes and methods
}
\examples{

}
\keyword{programming}
\keyword{interface}
