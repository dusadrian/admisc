\name{getName}

\alias{getName}

\title{Get the name of the object being used in a function call}

\description{
This is a utility to be used inside a function.
}

\usage{
getName(x, object = FALSE)
}

\arguments{
    \item{x}{String, expression to be evaluated}
    \item{object}{Logical, return the object's name}
}

\details{
Within a function, the argument \code{x} can be anything and it is usually
evaluated as an object.

This function should be used in conjunction with the base \code{match.call()},
to obtain the original name of the object being served as an input, regardless
of how it is being served.

A particular use case of this function relates to the cases when a variable
within a data.frame is used. The overall name of the object (the data frame)
is irrelevant, as the real object of interest is the variable.
}


\value{
A character vector of length 1.
}

\author{
Adrian Dusa
}

\examples{
foo <- function(x) {
    funargs <- sapply(match.call(), deparse)[-1]
    return(getName(funargs[1]))
}

dd <- data.frame(X = 1:5, Y = 1:5, Z = 1:5)

foo(dd)
# dd

foo(dd$X)
# X

foo(dd[["X"]])
# X

foo(dd[[c("X", "Y")]])
# X Y

foo(dd[, 1])
# X

foo(dd[, 2:3])
# Y Z
}

\keyword{functions}
