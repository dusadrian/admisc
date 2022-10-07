\name{inside}

\alias{inside}
\alias{inside.list}

\title{Evaluate an Expression in a Data Environment}

\description{
Evaluate an R expression in an environment constructed from data.
}

\usage{
inside(data, expr, ...)

\S3method{inside}{list}(data, expr, keepAttrs = TRUE, \dots)
}

\arguments{
    \item{data}{Data to use for constructing an environment a \code{data frame}
        or a \code{list}.}
    \item{expr}{Expression to evaluate, often a \dQuote{compound} expression,
        i.e., of the form \preformatted{
            {
                a <- somefun()
                b <- otherfun()
                .....
                rm(unused1, temp)
            }
        }}
    
    \item{keepAttrs}{For the \code{\link{list}} method of \code{inside()},
        a \code{\link{logical}} specifying if the resulting list should keep
        the \code{\link{attributes}} from \code{data} and have its
        \code{\link{names}} in the same order.  Often this is unneeded as
        the result is a \emph{named} list anyway, and then \code{keepAttrs =
        FALSE} is more efficient.}
    \item{...}{Arguments to be passed to (future) methods.}
}

\details{
This is a modified version of the base R function \code{within))}, with exactly
the same arguments and functionality but only one fundamental difference:
instead of returning a modified copy of the input data, this function alters the
data directly.
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
