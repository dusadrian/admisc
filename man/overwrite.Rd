\name{overwrite}

\alias{overwrite}

\title{
Overwrite an object in a given environment.
}

\description{
Utility function to overwrite an object, and bypass the assignment operator.
}

\usage{
overwrite(objname, content, environment)
}

\arguments{
    \item{objname}{Character, the name of the object to overwrite.}
    \item{content}{An R object}
    \item{environment}{The environment where to perform the overwrite procedure.}
}

\value{
This function does not return anything.
}

\author{
Adrian Dusa
}

\examples{
foo <- function(object, x) {
    objname <- deparse(substitute(object))
    object <- x
    overwrite(objname, object, parent.frame())
}


bar <- 1
foo(bar, 2)

bar
# [1] 2

bar <- list(A = bar)
foo(bar$A, 3)

bar
}

\keyword{functions}
