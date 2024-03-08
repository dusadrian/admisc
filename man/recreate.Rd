\name{recreate}
\alias{recreate}

\title{Facilitate expression substitution}

\description{
Utility function based on \code{substitute()}, to recover an unquoted input.
}

\usage{
recreate(x, snames = NULL, ...)
}

\arguments{
  \item{x}{A substituted input.}
  \item{snames}{A character string containing set names.}
  \item{...}{Other arguments, mainly for internal use.}
}

\details{
This function is especially useful when users have to provide lots of quoted
inputs, such as the name of the columns from a data frame to be considered
for a particular function.

This is actually one of the main uses of the base function
\bold{\code{\link[base]{substitute}()}}, but here it can be employed to also
detect SOP (sum of products) expressions, explained for instance in function
\bold{\code{\link{translate}()}}.

Such SOP expressions are usually used in contexts of sufficieny and necessity,
which are indicated with the usual signs \code{->} and \code{<-}. These are
both allowed by the R parser, indicating standard assignment. Due to the R's
internal parsing system, a sufficient expression using \code{->} is automatically
flipped to a necessity statement \code{<-} with reversed LHS to RHS, but this
function is able to determine what is the expression and what is the output.

The other necessity code \code{<=} is also recognized, but the equivalent
sufficiency code \code{=>} is not allowed in unquoted expressions.
}

\value{
A quoted, equivalent expression or a substituted object.
}

\author{
Adrian Dusa
}

\seealso{\code{\link[base]{substitute}}, \code{\link{simplify}}}

\examples{
recreate(substitute(A + ~B*C))

foo <- function(x, ...) recreate(substitute(list(...)))

foo(arg1 = 3, arg2 = A + ~B*C)

df <- data.frame(A = 1, B = 2, C = 3, Y = 4)

# substitute from the global environment
# the result is the builtin C() function
res <- recreate(substitute(C))

is.function(res) # TRUE

# search first within the column name space from df
recreate(substitute(C), colnames(df))
# "C"

# necessity well recognized
recreate(substitute(A <- B))

# but sufficiency is flipped
recreate(substitute(A -> B))

# more complex SOP expressions are still recovered
recreate(substitute(A + ~B*C -> Y))
}

\keyword{functions}
