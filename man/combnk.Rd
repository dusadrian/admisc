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
    \item{n}{Numeric scalar.}
    \item{k}{Numeric scalar.}
    \item{ogte}{At least one value greater than or equal to this number.}
    \item{zerobased}{Logical, zero or one based.}
}

\details{
This function differs from function \bold{\code{combn}} in package \bold{\code{utils}},
covering only numerical vectors and therefore a lot faster.

When the argument \bold{\code{ogte}} is specified, the combinations will sequentially
be incremented from those which contain that number.
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
}

\keyword{functions}
