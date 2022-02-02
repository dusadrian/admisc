\name{Number equality}

\alias{agtb}
\alias{altb}
\alias{agteb}
\alias{alteb}
\alias{aeqb}
\alias{aneqb}

\title{Check difference and / or (in)equality of numbers}

\description{
Check if one number is greater / lower than (or equal to) another.
}

\usage{
agtb(a, b, bincat)
altb(a, b, bincat)
agteb(a, b, bincat)
alteb(a, b, bincat)
aeqb(a, b, bincat)
aneqb(a, b, bincat)
}

\arguments{
  \item{a}{Numerical vector}
  \item{b}{Numerical vector}
  \item{bincat}{Binary categorization values, an atomic vector of length 2}
} 

\details{
Not all numbers (especially the decimal ones) can be represented exactly in
floating point arithmetic, and their arithmetic may not give the normal expected
result.

This set of functions check for the in(equality) between two numerical vectors a
and b, with the following name convention:

\bold{\code{gt}} means \dQuote{greater than}

\bold{\code{lt}} means a \dQuote{lower than} b

\bold{\code{gte}} means a \dQuote{greater than or equal to} b

\bold{\code{lte}} means a \dQuote{lower than or equal to} b

\bold{\code{eq}} means a \dQuote{equal to} b

\bold{\code{neq}} means a \dQuote{not equal to} b

The argument \bold{\code{values}} is useful to replace the TRUE / FALSE values
with custom categories.
}

\author{
Adrian Dusa
}


\references{
Goldberg, David (1991) "What Every Computer Scientist Should Know About
Floating-point Arithmetic", ACM Computing Surveys vol.23, no.1, pp.5-48,
\doi{10.1145/103162.103163}
}


\keyword{functions}
