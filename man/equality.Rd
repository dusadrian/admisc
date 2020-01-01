\name{Number equality}

\alias{agteb}
\alias{alteb}
\alias{aeqb}

\title{Check difference and / or equality of numbers}

\description{
Check if one number is greater/lower than or equal to another.
}

\usage{
agteb(a, b)
alteb(a, b)
aeqb(a, b)
}

\arguments{
  \item{a}{Numerical vector of numbers}
  \item{b}{Numerical vector of numbers}
}

\details{
Not all numbers (especially the decimal ones) can be represented exactly in floating point
arithmetic, and their arithmetic may not give the normal expected result.

Function \bold{\code{agteb()}} checks if a number \code{a} is \dQuote{greater than or equal to}
(\code{gte}) another number \code{b}. In the same spirit, function \bold{\code{alteb()}} checks
if a number \code{a} is \dQuote{lower than or equal to} (\code{lte}) another number \code{b},
while function \bold{\code{aeqb()}} checks for their equality (\code{eq}).
}

\author{
Adrian Dusa
}


\references{
Goldberg, David (1991) "What Every Computer Scientist Should Know About Floating-point Arithmetic",
ACM Computing Surveys vol.23, no.1, pp.5-48, DOI:
\href{http://dx.doi.org/10.1145/103162.103163}{10.1145/103162.103163}
}


\keyword{functions}
