\name{change}

\alias{change}

\title{
Generic function to change the structure of an object, function of the (changed)
parameters used to create it.
}

\description{
A generic function that applies different altering methods for different types
of objects (of certain classes).
}

\usage{
change(x, ...)
}

\arguments{
    \item{x}{An object of a particular class.}
    \item{...}{Arguments to be passed to a specific method.}
}

\details{
For the time being, this function is designed to change truth table objects (only).
Future versions will likely add class methods for different other objects.
}

\value{
    The changed object.
}

\author{
Adrian Dusa
}

\examples{
\dontrun{
# An example to change a QCA truth table
library(QCA)

ttLF <- truthTable(LF, outcome = SURV, incl.cut = 0.8)
minimize(ttLF, include = "?")

# excluding contradictory simplifying assumptions
minimize(
    change(ttLF, exclude = findRows(type = 2)),
    include = "?"
)
}
}

\keyword{functions}
