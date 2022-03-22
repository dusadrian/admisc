\name{numdec}
\alias{numdec}


\title{Count number of decimals}

\description{
Calculates the (maximum) number of decimals in a possibly numeric vector.
}

\usage{
numdec(x, each = FALSE, na.rm = TRUE, maxdec = 15)
}

\arguments{
  \item{x}{A vector of values}
  \item{each}{Logical, return the result for each value in the vector}
  \item{na.rm}{Logical, ignore missing values}
  \item{maxdec}{Maximal number of decimals to count}
}


\author{
Adrian Dusa
}

\examples{
x <- c(12, 12.3, 12.34)

numdec(x) # 2

numdec(x, each = TRUE) # 0, 1, 2

x <- c("-.1", " 2.75 ", "12", "B", NA)

numdec(x) # 2

numdec(x, each = TRUE) # 1, 2, 0, NA, NA
}


\keyword{functions}
