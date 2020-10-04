\name{finvert}
\alias{finvert}

\title{Inverts the values of a factor}
\description{
  Useful function to invert the values from a categorical variable, for instance
  a Likert response scale.}
\usage{
finvert(variable, levels = FALSE)
}

\arguments{
  \item{variable}{A categorical variable (a factor)}
  \item{levels}{Logical, invert the levels as well}
}

\value{A factor of the same length as the original one.}

\author{Adrian Dusa}

\seealso[stats]{relevel}

\examples{
words <- c("ini", "mini", "miny", "moe")
variable <- factor(words, levels = words)

# inverts the value, preserving the levels
finvert(variable)

# inverts both values and levels
finvert(variable, levels = TRUE)

}

\keyword{misc}

