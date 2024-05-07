\name{Brackets}

\alias{insideBrackets}
\alias{betweenBrackets}
\alias{outsideBrackets}
\alias{curlyBrackets}
\alias{squareBrackets}
\alias{roundBrackets}

\title{Extract information from a multi-value SOP/DNF expression}

\description{
Functions to extract information from an expression written in SOP - sum of products form,
(or from the canonical DNF - disjunctive normal form) for multi-value causal conditions.
It extracts either the values within brackets, or the causal conditions' names outside the
brackets.
}

\usage{
betweenBrackets(x, type = "[", invert = FALSE, regexp = NULL)
outsideBrackets(x, type = "[", regexp = NULL)
curlyBrackets(x, outside = FALSE, regexp = NULL)
squareBrackets(x, outside = FALSE, regexp = NULL)
roundBrackets(x, outside = FALSE, regexp = NULL)
}

\arguments{
  \item{x}{A DNF/SOP expression.}
  \item{type}{Brackets type: curly, round or square.}
  \item{invert}{Logical, if activated returns whatever is not within the brackets.}
  \item{outside}{Logical, if activated returns the conditions' names outside the brackets.}
  \item{regexp}{Optional regular expression to extract information with.}
}

\details{
Expressions written in SOP - sum of products are used in Boolean logic, signaling a
disjunction of conjunctions.

These expressions are useful in Qualitative Comparative Analysis, a social science methodology
that is employed in the context of searching for causal configurations that are associated
with a certain outcome.

They are also used to draw Venn diagrams with the package \bold{\code{venn}}, which draws any
kind of set intersection (conjunction) based on a custom SOP expression.

The functions \bold{\code{curlyBrackets}}, \bold{\code{squareBrackets}} and
\bold{\code{roundBrackets}} are just special cases of the functions \bold{\code{betweenBrackets}}
and \bold{\code{outsideBrackets}}, using the argument \bold{\code{type}} as either
\code{"{"}, \code{"["}  or \code{"("}.

The function \bold{\code{outsideBrackets}} itself can be considered a special case of the
function \bold{\code{betweenBrackets}}, when it uses the argument \bold{\code{invert = TRUE}}.

SOP expressions are usually written using curly brackets for multi-value conditions but to allow
the evaluation of unquoted expressions, they first needs to get past R's internal parsing system.
For this reason, multi-value conditions in unquoted expresions should use the square brackets
notation, and conjunctions should always use the product \code{*} sign.

Sufficiency is recognized as \code{"=>"} in quoted expressions but this does not pass over R's
parsing system in unquoted expressions. To overcome this problem, it is best to use the single
arrow \code{"->"} notation. Necessity is recognized as either \code{"<="} or \code{"<-"}, both
being valid in quoted and unquoted expressions.
}

\author{
Adrian Dusa
}

\examples{
sop <- "A[1] + B[2]*C[0]"

betweenBrackets(sop) # 1, 2, 0

betweenBrackets(sop, invert = TRUE) # A, B, C

# unquoted (valid) SOP expressions are allowed, same result
betweenBrackets(A[1] + B[2]*C[0]) # the default type is "["

# curly brackets are also valid in quoted expressions
betweenBrackets("A{1} + B{2}*C{0}", type = "{")

# or
curlyBrackets("A{1} + B{2}*C{0}")

# and the condition names
curlyBrackets("A{1} + B{2}*C{0}", outside = TRUE)

squareBrackets(A[1] + B[2]*C[0]) # 1, 2, 0

squareBrackets(A[1] + B[2]*C[0], outside = TRUE) # A, B, C
}

\keyword{functions}
