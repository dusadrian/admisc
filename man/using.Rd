\name{using}

\alias{using}
\alias{using.data.frame}

\title{Evaluate an expression in a data environment}

\description{
A function almost identical to the base function \code{with()}, but allowing
to evaluate the expression in every subset of a split file.
}

\usage{
using(data, expr, split.by = NULL, ...)
}

\arguments{
    \item{data}{A data frame.}
    \item{expr}{Expression to evaluate}
    \item{split.by}{A factor variable from the \code{data}, or a \code{declared}/\code{labelled} variable}
    \item{...}{Other internal arguments.}
}

\value{
A list of results, or a matrix if each separate result is a vector.
}


\author{
Adrian Dusa
}

\examples{
set.seed(123)
DF <- data.frame(
    Area = factor(sample(c("Rural", "Urban"), 123, replace = TRUE)),
    Gender = factor(sample(c("Female", "Male"), 123, replace = TRUE)),
    Age = sample(18:90, 123, replace = TRUE),
    Children = sample(0:5, 123, replace = TRUE)
)


# table of frequencies for Gender
table(DF$Gender)

# same with
using(DF, table(Gender))

# same, but split by Area
using(DF, table(Gender), split.by = Area)

# calculate the mean age by gender
using(DF, mean(Age), split.by = Gender)

# same, but select cases from the urban area
using(subset(DF, Area == "Urban"), mean(Age), split.by = Gender)

# mean age by gender and area
using(DF, mean(Age), split.by = Area & Gender)

# same with
using(DF, mean(Age), split.by = c(Area, Gender))

# average number of children by Area
using(DF, mean(Children), split.by = Area)

# frequency tables by Area
using(DF, table(Children), split.by = Area)
}

\keyword{functions}
