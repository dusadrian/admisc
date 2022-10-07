\name{inside}

\alias{inside}

\title{Evaluate an Expression in a Data Environment}

\description{
Evaluate an R expression in an environment constructed from data.
}

\usage{
inside(data, expr, ...)
}

\arguments{
    \item{data}{Data to use for constructing an environment. For the moment, a
    \code{data frame}.}
    \item{expr}{expression to evaluate}
    \item{...}{arguments to be passed to (future) methods.}
}

\details{
This is a generic function. Unlike its modified verion \code{within))}, this
function \bold{does} change the object.
}

\author{
Adrian Dusa
}

\examples{
mt <- mtcars
inside(mt, hwratio <- hp/wt)

dim(mtcars)

dim(mt)
}

\keyword{functions}
