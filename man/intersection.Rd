\name{intersection}

\alias{intersection}

\title{Intersect expressions}

\description{
This function takes two or more SOP expressions (combinations of conjunctions and
disjunctions) or even entire minimization objects, and finds their intersection.
}

\usage{
intersection(..., snames = "", noflevels)
}

\arguments{
  \item{...}{One or more expressions, combined with / or minimization objects
  of class \code{"QCA_min"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  
}

\details{
The initial aim of this function was to provide a software implementation of the
intersection examples presented by Ragin (1987: 144-147). That type of example can also
be performed with the function \bold{\code{simplify()}}, while this
function is now mainly used in conjunction with the \bold{\code{\link[QCA]{modelFit}()}}
function from package \bold{\pkg{QCA}}, to assess the intersection between theory and a
QCA model.

Irrespective of the input type (character expressions and / or minimiation objects),
this function is now a wrapper to the main \bold{\code{simplify()}}
function (which only accepts character expressions).

It can deal with any kind of expressions, but multivalent crisp conditions need additional
information about their number of levels, via the argument \bold{\code{noflevels}}.

The expressions can be formulated in terms of either lower case - upper case notation
for the absence and the presence of the causal condition, or use the tilde notation
(see examples below). Usage of either of these is automatically detected, as long as all
expressions use the same notation.

If the \bold{\code{snames}} argument is provided, the result is sorted according to the order
of the causal conditions (set names) in the original dataset, otherwise it sorts the causal
conditions in alphabetical order.

For minimzation objects of class \code{"QCA_min"}, the number of levels, and the set names are
automatically detected.
}

\author{
Adrian Dusa
}


\references{
Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.
}


\examples{
# using minimization objects
\dontrun{
library(QCA) # if not already loaded
ttLF <- truthTable(LF, outcome = "SURV", incl.cut = 0.8)
pLF <- minimize(ttLF, include = "?")


# for example the intersection between the parsimonious model and
# a theoretical expectation
intersection(pLF, DEV*STB)


# negating the model
intersection(negate(pLF), DEV*STB)
}


# -----
# in Ragin's (1987) book, the equation E = SG + LW is the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = lw)
# with the equation E (page 144)
intersection(~L~W, SG + LW, snames = c(S, L, W, G))


# resources for size and wealth (C = SW) with E (page 145)
intersection(SW, SG + LW, snames = c(S, L, W, G))


# and factorized
factorize(intersection(SW, SG + LW, snames = c(S, L, W, G)))


# developmental perspective (D = L~G) and E (page 146)
intersection(L~G, SG + LW, snames = c(S, L, W, G))


# subnations that exhibit ethic political mobilization (E) but were
# not hypothesized by any of the three theories (page 147)
# ~H = ~(~L~W + SW + L~G)
intersection(negate(~L~W + SW + L~G), SG + LW, snames = c(S, L, W, G))
}

\keyword{functions}

