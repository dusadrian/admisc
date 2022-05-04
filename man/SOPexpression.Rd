\name{Interpret DNF/SOP expressions: compute, simplify, expand, translate}

\alias{asSOP}
\alias{compute}
\alias{expand}
\alias{mvSOP}
\alias{simplify}
\alias{sop}
\alias{translate}

\title{Functions to interpret and manupulate a SOP/DNF expression}

\description{
These functions interpret an expression written in sum of products (SOP) or in
canonical disjunctive normal form (DNF), for both crisp and multivalue notations.
The function \bold{\code{compute()}} calculates set membership scores based on a
SOP expression applied to a calibrated data set (see function
\bold{\code{\link[QCA]{calibrate}()}} from package \bold{\pkg{QCA}}), while the
function \bold{\code{translate()}} translates a  SOP expression into a matrix form.

The function \bold{\code{simplify()}} transforms a SOP expression into a simpler
equivalent, through a process of Boolean minimization. The package uses the
function \bold{\code{\link[QCA]{minimize}()}} from package \bold{\pkg{QCA}}), so
users are highly encouraged to install and load that package, despite not being
present in the Imports field (due to circular dependency issues).

Function \bold{\code{expand()}} performs a Quine expansion to the complete DNF,
or a partial expansion to a SOP expression with equally complex terms.

Function \bold{\code{asSOP()}} returns a SOP expression from a POS (product of
sums) expression. This function is different from the function
\bold{\code{invert()}}, which also negates each causal condition.

Function \bold{\code{mvSOP()}} coerces an expression from crisp set notation to
multi-value notation.
}

\usage{
asSOP(expression = "", snames = "", noflevels = NULL)

compute(expression = "", data = NULL, separate = FALSE, ...)

expand(expression = "", snames = "", noflevels = NULL, partial = FALSE,
      implicants = FALSE, ...)

mvSOP(expression = "", snames = "", data = NULL, keep.tilde = TRUE, ...)

simplify(expression = "", snames = "", noflevels = NULL, ...)

translate(expression = "", snames = "", noflevels = NULL, data = NULL, ...)
}

\arguments{
  \item{expression}{String, a SOP expression.}
  \item{data}{A dataset with binary cs, mv and fs data.}
  \item{separate}{Logical, perform computations on individual, separate paths.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{partial}{Logical, perform a partial Quine expansion.}
  \item{implicants}{Logical, return an expanded matrix in the implicants space.}
  \item{keep.tilde}{Logical, preserves the tilde sign when coercing a factor level}
  \item{...}{Other arguments, mainly for backwards compatibility.}
}

\details{
An expression written in sum of products (SOP), is a "union of intersections",
for example \bold{\code{A*B + B*~C}}. The disjunctive normal form (DNF) is also
a sum of products, with the restriction that each product has to contain all
literals. The equivalent DNF expression is: \bold{\code{A*B*~C + A*B*C + ~A*B*~C}}

The same expression can be written in multivalue notation:
\bold{\code{A[1]*B[1] + B[1]*C[0]}}.

Expressions can contain multiple values for the same condition, separated by a
comma. If B was a multivalue causal condition, an expression could be:
\bold{\code{A[1] + B[1,2]*C[0]}}.

Whether crisp or multivalue, expressions are treated as Boolean. In this last
example, all values in B equal to either 1 or 2 will be converted to 1, and the
rest of the (multi)values will be converted to 0.

Negating a multivalue condition requires a known number of levels (see examples
below). Intersections between multiple levels of the same condition are possible.
For a causal condition with 3 levels (0, 1 and 2) the following expression
\bold{\code{~A[0,2]*A[1,2]}} is equivalent with \bold{\code{A[1]}}, while
\bold{\code{A[0]*A[1]}} results in the empty set.

The number of levels, as well as the set names can be automatically detected
from a dataset via the argument \bold{\code{data}}. When specified, arguments
\bold{\code{snames}} and \bold{\code{noflevels}} have precedence over
\bold{\code{data}}.

The product operator \bold{\code{*}} should always be used, but it can be omitted
when the data is multivalue (where product terms are separated by curly brackets),
and/or when the set names are single letters (for example \bold{\code{AD + B~C}}),
and/or when the set names are provided via the argument \bold{\code{snames}}.

When expressions are simplified, their simplest equivalent can result in the
empty set, if the conditions cancel each other out.

The function \bold{\code{mvSOP()}} assumes binary crisp conditions in the
expression, except for categorical data used as multi-value conditions. The 
factor levels are read directly from the data, and they should be unique accross
all conditions.
}


\value{
For the function \bold{\code{compute()}}, a vector of set membership values.

For function \bold{\code{simplify()}}, a character expression.

For the function \bold{\code{translate()}}, a matrix containing the implicants
on the rows and the set names on the columns, with the following codes:
\tabular{rl}{
     0 \tab absence of a causal condition\cr
     1 \tab presence of a causal condition\cr
    -1 \tab causal condition was eliminated
}
The matrix was also assigned a class "translate", to avoid printing the -1 codes
when signaling a minimized condition. The mode of this matrix is character, to
allow printing multiple levels in the same cell, such as "1,2".

For function \bold{\code{expand()}}, a character expression or a matrix of
implicants.
}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.
}

\examples{
# -----
# for compute()
\dontrun{
# make sure the package QCA is loaded
library(QCA)
compute(DEV*~IND + URB*STB, data = LF)

# calculating individual paths
compute(DEV*~IND + URB*STB, data = LF, separate = TRUE)
}


# -----
# for simplify(), also make sure the package QCA is loaded
simplify(asSOP("(A + B)(A + ~B)")) # result is "A"

# works even without the quotes
simplify(asSOP((A + B)(A + ~B))) # result is "A"

# but to avoid confusion POS expressions are more clear when quoted
# to force a certain order of the set names
simplify("(URB + LIT*~DEV)(~LIT + ~DEV)", snames = c(DEV, URB, LIT))

# multilevel conditions can also be specified (and negated)
simplify("(A[1] + ~B[0])(B[1] + C[0])", snames = c(A, B, C), noflevels = c(2, 3, 2))


# Ragin's (1987) book presents the equation E = SG + LW as the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = ~L~W)
# with the equation E (page 144)

simplify("~L~W(SG + LW)", snames = c(S, L, W, G))

# [1] "S~L~WG"


# resources for size and wealth (C = SW) with E (page 145)
simplify("SW(SG + LW)", snames = c(S, L, W, G))

# [1] "SWG + SLW"


# and factorized
factorize(simplify("SW(SG + LW)", snames = c(S, L, W, G)))

# F1: SW(G + L)


# developmental perspective (D = Lg) and E (page 146)
simplify("L~G(SG + LW)", snames = c(S, L, W, G))

# [1] "LW~G"

# subnations that exhibit ethnic political mobilization (E) but were
# not hypothesized by any of the three theories (page 147)
# ~H = ~(~L~W + SW + L~G) = GL~S + GL~W + G~SW + ~L~SW

simplify("(GL~S + GL~W + G~SW + ~L~SW)(SG + LW)", snames = c(S, L, W, G))


# -----
# for translate()
translate(A + B*C)

# same thing in multivalue notation
translate(A[1] + B[1]*C[1])

# tilde as a standard negation (note the condition "b"!)
translate(~A + b*C)

# and even for multivalue variables
# in multivalue notation, the product sign * is redundant
translate(C[1] + T[2] + T[1]*V[0] + C[0])

# negation of multivalue sets requires the number of levels
translate(~A[1] + ~B[0]*C[1], snames = c(A, B, C), noflevels = c(2, 2, 2))

# multiple values can be specified
translate(C[1] + T[1,2] + T[1]*V[0] + C[0])

# or even negated
translate(C[1] + ~T[1,2] + T[1]*V[0] + C[0], snames = c(C, T, V), noflevels = c(2,3,2))

# if the expression does not contain the product sign *
# snames are required to complete the translation 
translate(AaBb + ~CcDd, snames = c(Aa, Bb, Cc, Dd))

# to print _all_ codes from the standard output matrix
(obj <- translate(A + ~B*C))
print(obj, original = TRUE) # also prints the -1 code


# -----
# for expand()
expand(~AB + B~C)

# S1: ~AB~C + ~ABC + AB~C 

expand(~AB + B~C, snames = c(A, B, C, D))

# S1: ~AB~C~D + ~AB~CD + ~ABC~D + ~ABCD + AB~C~D + AB~CD 

# In implicants form:
expand(~AB + B~C, snames = c(A, B, C, D), implicants = TRUE)

#      A B C D
# [1,] 1 2 1 1    ~AB~C~D
# [2,] 1 2 1 2    ~AB~CD
# [3,] 1 2 2 1    ~ABC~D
# [4,] 1 2 2 2    ~ABCD
# [5,] 2 2 1 1    AB~C~D
# [6,] 2 2 1 2    AB~CD

}

\keyword{functions}
