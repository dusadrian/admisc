\name{frev}
\alias{frev}
\alias{finvert}

\title{Inverts the values of a factor}
\description{
  Provides a reversed version of the values from a factor, for instance
  a Likert type response scale.}
\usage{
frev(x, labels = FALSE)
}

\arguments{
  \item{x}{A factor}
  \item{labels}{Logical, invert the labels as well}
}

\details{
  The argument \code{labels} can also be used for the levels of a factor.
}

\value{A factor of the same length as the original one.}

\author{Adrian Dusa}

\examples{
words <- c("ini", "mini", "miny", "moe")
variable <- factor(words, labels = words)

# inverts the values, preserving the labels' order
frev(variable)

# inverts both values and labels
frev(variable, labels = TRUE)

}

\keyword{misc}

