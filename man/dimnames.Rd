\name{dimnames}
\alias{setColnames}
\alias{setRownames}
\alias{setDimnames}


\title{Set matrix row or column names}

\description{
Set matrix row or column names without copying, especially useful for (very)
large matrices.
}

\usage{
setColnames(matrix, colnames)
setRownames(matrix, rownames)
setDimnames(matrix, nameslist)
}

\arguments{
  \item{matrix}{An R matrix}
  \item{colnames}{Character vector of column names}
  \item{rownames}{Character vector of row names}
  \item{nameslist}{A two-component list containing rownames and colnames}
}


\author{
Adrian Dusa
}

\examples{

mat <- matrix(1:9, nrow = 3)
setDimnames(mat, list(LETTERS[1:3], letters[1:3]))
}


\keyword{functions}
