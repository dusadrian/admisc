\name{frev}
\alias{frev}
\alias{finvert}

\title{Inverts the values of a factor}
\description{
  Provides a reversed version of the values from a factor, for instance
  a Likert type response scale.}
\usage{
frev(x, levels = FALSE)
}

\arguments{
  \item{x}{A factor}
  \item{levels}{Logical, invert the levels as well}
}

\value{A factor of the same length as the original one.}

\author{Adrian Dusa}

\examples{
words <- c("ini", "mini", "miny", "moe")
variable <- factor(words, levels = words)

# inverts the value, preserving the levels
frev(variable)

# inverts both values and levels
frev(variable, levels = TRUE)

}

\keyword{misc}

