\name{combnk}

\alias{combnk}

\title{Generate all combinations of n numbers, taken k at a time}

\description{
A fast function to generate all possible combinations of n numbers, taken k at a time,
starting from the first k numbers or starting from a combination that contain a
certain number.
}

\usage{
combnk(n, k, ogte = 0, zerobased = FALSE)
}

\arguments{
    \item{n}{Vector of any kind, or a numerical scalar.}
    \item{k}{Numeric scalar.}
    \item{ogte}{At least one value greater than or equal to this number.}
    \item{zerobased}{Logical, zero or one based.}
}

\details{
When a scalar, argument \code{n} should be numeric, otherwise when a vector its
length should not be less than \code{k}.

When the argument \bold{\code{ogte}} is specified, the combinations will sequentially
be incremented from those which contain a certain number, or a certain position from
\code{n} when specified as a vector.
}


\value{
A matrix with \code{k} rows and \code{choose(n, k)} columns.
}

\author{
Adrian Dusa
}

\examples{
combnk(5, 2)

combnk(5, 2, ogte = 3)

combnk(letters[1:5], 2)
}

\keyword{functions}
