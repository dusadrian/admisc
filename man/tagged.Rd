\name{Tagged NAs}

\alias{tag_na}
\alias{has_tag}
\alias{get_tag}

\title{Set, check and extract tagged NAs}

\description{
Improved versions of the original functions \code{tagged_na()}, \code{is_tagged_na()} and
\code{na_tag()} from package \bold{\code{haven}}.
}

\usage{
tag_na(...)

has_tag(x, tag = NULL)

get_tag(x)
}

\arguments{
  \item{...}{Vectors containing the tags for the missing value}
  \item{x}{A numeric vector}
  \item{tag}{If \code{NULL}, will only return true if the tag has this value.}
}

\details{
Tagged missing values are very useful for social scientists, to signal that a certain
value should be treated as if it was missing. Answers such as \dquote{Don't know}, or
\dquote{Not applicable} are usually allocated numeric codes that are very different from
the regular values in a variable, and should not be taken into account when calculating
summary measures (among others).

Apart from the regular missing values in R (signaled with \code{NA}), package
\bold{\code{haven}} introduced the concept of tagged NAs, by storing one additional byte
of information in the NA value.

This is extremely useful, but the \code{haven} functions have limitations. The first and
the most obvious, it accepts only one character as a tag. However, as it turns out, it is
actually possible to store two additional bytes instead of only one.

This makes it possible to store not only characters, but the numbers themselves as "tags",
as long as their absolute value is below 100. For higher numbers, only the first two
digits are used.

Tagged NA values are printed on the R console as regular NAs. Package \bold{\code{haven}}
has a dedicated printing method that displays (for instance) \code{NA(a)}, while users
preferring a Stata type of display can load the package \bold{\code{mixed}}, which has
the additional benefit that stores missing values as proper NA values.
}

\author{
Adrian Dusa
}

\value{
If all tags are numeric, the function \code{get_tag()} will return a numeric vector,
otherwise it returns a character vector.
}

\references{
Goldberg, David (1991) "What Every Computer Scientist Should Know About Floating-point Arithmetic",
ACM Computing Surveys vol.23, no.1, pp.5-48, \doi{10.1145/103162.103163}
}

\examples{
x <- c(1:5, tag_na("a"), tag_na(-98), NA)
x
# [1]  1  2  3  4  5 NA NA NA

is.na(x)
# [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE

has_tag(x)
# [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE

has_tag(x, -98)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE

get_tag(x)
# [1] NA    NA    NA    NA    NA    "a"   "-98" NA

get_tag(tag_na("ab"))
# [1] "ab"

get_tag(tag_na(-98))
# [1] -98
}


\keyword{functions}
