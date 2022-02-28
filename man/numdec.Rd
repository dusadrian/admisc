\name{numdec}
\alias{numdec}


\title{Number of decimals}

\description{
Calculates the (maximum) number of decimals in a possibly numeric vector.
}

\usage{
numdec(x, each = FALSE, na.rm = TRUE)
}

\arguments{
  \item{x}{A vector of values}
  \item{each}{Logical, return the result for each value in the vector}
  \item{na.rm}{Logical, ignore missing values}
}


\author{
Adrian Dusa
}

\examples{
x <- c(12, 12.3, 12.34)

numdec(x) # 2

numdec(x, each = TRUE) # 0, 1, 2

x <- c("-.1", " 2.75 ", "12")

numdec(x) # 2

numdec(x, each = TRUE) # 1, 2, 0

x <- c("-.1", " 2.75 ", "B")

# numdec(x) will result into an error that x is not numeric

numdec(x, each = TRUE) # 1 2 NA
}


\keyword{functions}
