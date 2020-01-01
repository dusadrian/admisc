\name{Interpret DNF/SOP expressions: compute, expand, simplify, translate}

\alias{compute}
\alias{expand}
\alias{simplify}
\alias{sop}
\alias{translate}

\title{Functions to interpret and manupulate a SOP/DNF expression}

\description{
These functions interpret an expression written in a SOP - sum of products (or in canonical
DNF - disjunctive normal form), for both crisp and multivalue notations. The function
\bold{\code{compute()}} calculates set membership scores based on a SOP expression applied
to a calibrated data set (see function \bold{\code{\link[QCA]{calibrate}()}}
from package \bold{\pkg{QCA}}), while the function \bold{\code{translate()}} translates a 
SOP expression into a matrix form.

For crisp sets notation, upper case letters are considered the presence of that causal
condition, and lower case letters are considered the absence of the respective causal
condition. Tilde is recognized as a negation, even in combination with upper/lower letters.

Functions similar to \bold{\code{translate()}} and \bold{\code{compute()}} have initially
been written by Lewandowski (2015) but the actual code in these functions has been
completely re-written and expanded with more extensive functionality (see details and
examples below).

The function \bold{\code{simplify()}} transforms such a SOP expression (and even a POS -
product of sums) into a simpler sum of products, minimizing it to the simplest equivalent
logical expression. It also provides a software implementation of the intersection examples
presented by Ragin (1987: 144-147), and extended to multi-value sets. It uses the function
\bold{\code{\link[QCA]{minimize}()}} from package \bold{\pkg{QCA}}), so users are highly
encouraged to install and load that package, despite not being present in the Imports field
(due to circular dependency issues).

Function \bold{\code{expand()}} performs a Quine expansion to the complete DNF, or a
partial expansion to a SOP expression with equally complex terms.
}

\usage{
compute(expression = "", data, separate = FALSE)

simplify(expression = "", snames = "", noflevels = NULL, ...)

translate(expression = "", snames = "", noflevels = NULL, data, ...)

expand(expression, snames = "", noflevels = NULL, partial = FALSE,
      implicants = FALSE, ...)
}

\arguments{
  \item{expression}{String: a SOP - sum of products expression.}
  \item{data}{A dataset with binary cs, mv and fs data.}
  \item{separate}{Logical, perform computations on individual, separate paths.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{partial}{Logical, perform a partial Quine expansion.}
  \item{implicants}{Logical, return an expanded matrix in the implicants space.}
  \item{...}{Other arguments, mainly for backwards compatibility.}
}

\details{
A DNF - disjunctive normal form is also known as a SOP - sum of products, or in other
words a "union of intersections", for example \bold{\code{A*D + B*c}}.

The same expression can be written in multivalue notation: \bold{\code{A{1}*D{1} + B{1}*C{0}}}.
Both types of expressions are valid, and yield the same result on the same dataset.

For multivalue notation, causal conditions are expected as upper case letters, and they will be
converted to upper case by default. Expressions can contain multiple values to translate, separated
by a comma. If B was a multivalue causal condition, an expression could be:
\bold{\code{A{1} + B{1,2}*C{0}}}.

In this example, all values in B equal to either 1 or 2 will be converted to 1, and the
rest of the (multi)values will be converted to 0.

These functions automatically detects the use of tilde \dQuote{\code{~}} as a negation for a particular
causal condition. \bold{\code{~A}} does two things: it identifies the presence of causal
condition \bold{\code{A}} (because it was specified as upper case) and it recognizes that it
must be negated, because of the  tilde. It works even combined with lower case names:
\bold{\code{~a}}, which is interpreted as \bold{\code{A}}.

To negate a multivalue condition using a tilde, the number of levels should be supplied
(see examples below). Improvements in version 2.5 allow for intersections
between multiple levels of the same condition. For a causal condition with 3 levels (0, 1 and 2)
the following expression \bold{\code{~A{0,2}*A{1,2}}} is equivalent with \bold{\code{A{1}}}, while
\bold{\code{A{0}*A{1}}} results in the empty set.

The number of levels, as well as the set names can be automatically detected from a dataset via
the argument \bold{\code{data}}. Arguments \bold{\code{snames}} and \bold{\code{noflevels}} have
precedence over \bold{\code{data}}, when specified.

The use of the product operator \bold{\code{*}} is redundant when the set names are single
letters (for example \bold{\code{AD + Bc}}), and is also redundant for multivalue data, where 
product terms can be separated by using the curly brackets notation.

When conditions are binary and their names have multiple letters (for example
\bold{\code{AA + CC*bb}}), the use of the product operator \bold{\code{*}} is preferable but the
function manages to translate an expression even without it (\bold{\code{AA + CCbb}}) by searching
deep in the space of the conditions' names, at the cost of slowing down for a high number of causal
conditions. For this reason, an arbitrary limit of 7 causal \bold{\code{snames}} is imposed, to
write an expression.

For Boolean expressions, the simplest equivalent logical expression can result in the empty set,
if the conditions cancel each other out.
}


\value{
For the function \bold{\code{compute()}}, a vector of set membership values.

For function \bold{\code{simplify()}}, a character expression.

For the function \bold{\code{translate()}}, a matrix containing the implicants on the
rows and the set names on the columns, with the following codes:
\tabular{rl}{
     0 \tab absence of a causal condition\cr
     1 \tab presence of a causal condition\cr
    -1 \tab causal condition was eliminated
}
The matrix was also assigned a class "translate", to avoid printing the -1 codes when signaling
a minimized condition. The mode of this matrix is character, to allow printing multiple levels
in the same cell, such as "1,2".
}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Lewandowski, J. (2015) QCAtools: Helper functions for QCA in R. R package version 0.1
}

\examples{
# -----
# for compute()
\dontrun{
# make sure the package QCA is loaded
library(QCA)
data(LF)
compute("DEV*~IND + URB*STB", data = LF)

# calculating individual paths
compute("DEV*~IND + URB*STB", data = LF, separate = TRUE)
}


# -----
# for simplify(), also make sure the package QCA is loaded
simplify("(A + B)(A + ~B)") # result is "A"

# to force a certain order of the set names
simplify("(URB + LIT*~DEV)(~LIT + ~DEV)", snames = "DEV, URB, LIT")

# multilevel conditions can also be specified (and negated)
simplify("(A{1} + ~B{0})(B{1} + C{0})", snames = "A, B, C", noflevels = c(2, 3, 2))


# in Ragin's (1987) book, the equation E = SG + LW is the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = lw)
# with the equation E (page 144)

simplify("~L~W(SG + LW)", snames = "S, L, W, G")

# [1] "S~L~WG"


# resources for size and wealth (C = SW) with E (page 145)
simplify("SW(SG + LW)", snames = "S, L, W, G")

# [1] "SWG + SLW"


# and factorized
factorize(simplify("SW(SG + LW)", snames = "S, L, W, G"))

# F1: SW(G + L)


# developmental perspective (D = Lg) and E (page 146)
simplify("L~G(SG + LW)", snames = "S, L, W, G")

# [1] "LW~G"

# subnations that exhibit ethnic political mobilization (E) but were
# not hypothesized by any of the three theories (page 147)
# ~H = ~(~L~W + SW + L~G) = GL~S + GL~W + G~SW + ~L~SW

simplify("(GL~S + GL~W + G~SW + ~L~SW)(SG + LW)", snames = "S, L, W, G")


# -----
# for translate()
translate("A + B*C")

# same thing in multivalue notation
translate("A{1} + B{1}*C{1}")

# tilde as a standard negation (note the condition "b"!)
translate("~A + b*C")

# and even for multivalue variables
# in multivalue notation, the product sign * is redundant
translate("C{1} + T{2} + T{1}V{0} + C{0}")

# negation of multivalue sets requires the number of levels
translate("~A{1} + ~B{0}*C{1}", snames = "A, B, C", noflevels = c(2, 2, 2))

# multiple values can be specified
translate("C{1} + T{1,2} + T{1}V{0} + C{0}")

# or even negated
translate("C{1} + ~T{1,2} + T{1}V{0} + C{0}", snames = "C, T, V", noflevels = c(2,3,2))

# if the expression does not contain the product sign *
# snames are required to complete the translation 
translate("AaBb + ~CcDd", snames = "Aa, Bb, Cc, Dd")

# to print _all_ codes from the standard output matrix
(obj <- translate("A + ~B*C"))
print(obj, original = TRUE) # also prints the -1 code
}

\keyword{functions}
