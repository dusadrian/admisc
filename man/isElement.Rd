\name{Set membership}

\alias{isElement}

\title{Set membership}

\description{
Check if an element x is a member of a set y
}

\usage{
isElement(x, y)
}

\arguments{
  \item{x}{A scalar or a vector}
  \item{y}{A scalar or a vector}
}

\details{
This function extends the base \bold{\code{is.element()}} function to labelled
objects, especially when containing tagged NA values.

Normally \code{x} is a value and \code{y} is a set, but there is nothing to prevent
the other way round (see examples).

When used in the other way, it also extends the basic equality check using \code{==}.
The double equal operator is returns an \code{NA} when the value in \code{x} is NA,
which is sometimes not what we're asking.

In the same spirit, it can also replace the base function \bold{\code{is.na()}}, by
checking the set membership in the set containing a single NA value.

Both \code{x} and \code{y} can contain tagged NA values, as defined by package
\code{haven}.
}

\author{
Adrian Dusa
}

\examples{

isElement(3, 1:5) 

x <- 1:5

x == 3
# [1] FALSE FALSE  TRUE FALSE FALSE

isElement(x, 3)
# [1] FALSE FALSE  TRUE FALSE FALSE

# introducing a missing value
x <- c(1:5, NA)

x == 3
# [1] FALSE FALSE  TRUE FALSE FALSE    NA

isElement(x, 3)                   # !
# [1] FALSE FALSE  TRUE FALSE FALSE FALSE

is.na(x)
# [1] FALSE FALSE FALSE FALSE FALSE  TRUE

isElement(x, NA)
# [1] FALSE FALSE FALSE FALSE FALSE  TRUE

}


\keyword{functions}
