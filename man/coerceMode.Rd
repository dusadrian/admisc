\name{coerceMode}

\alias{coerceMode}

\title{Coerce an atomic vector to numeric or integer, if possible}

\description{
This function verifies if an R vector is possibly numeric, and further if the
numbers inside are whole numbers.
}

\usage{
coerceMode(x)
}

\arguments{
    \item{x}{An atomic R vector}
}

\value{
    An R vector of coerced mode.
}

\author{
Adrian Dusa
}

\examples{
obj <- c("1.0", 2:5)

is.integer(coerceMode(obj))
}

\keyword{functions}
