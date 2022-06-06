\name{factorize}

\alias{factorize}

\title{Factorize Boolean expressions}

\description{
This function finds all combinations of common factors in a Boolean expression
written in SOP - sum of products. It makes use of the function
\bold{\code{\link{simplify}()}}, which uses the function
\bold{\code{\link[QCA]{minimize}()}} from package \bold{\pkg{QCA}}). Users are
highly encouraged to install and load that package, despite not being present
in the Imports field (due to circular dependency issues).
}

\usage{
factorize(input, snames = "", noflevels = NULL, pos = FALSE, ...)
}

\arguments{
  \item{input}{A string representing a SOP expression, or a minimization
        object of class \code{"qca"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{pos}{Logical, if possible factorize using product(s) of sums.}
  \item{...}{Other arguments (mainly for backwards compatibility).}
}

\details{

Factorization is a process of finding common factors in a Boolean expression,
written in SOP - sum of products. Whenever possible, the factorization can also
be performed in a POS - product of sums form.

Conjunctions should preferably be indicated with a star \code{*} sign, but this is not
necessary when conditions have single letters or when the expression is expressed in
multi-value notation.

The argument \bold{\code{snames}} is only needed when conjunctions are not indicated by
any sign, and the set names have more than one letter each (see function
\bold{\code{\link{translate}()}} for more details).

The number of levels in \bold{\code{noflevels}} is needed only when negating multivalue
conditions, and it should complement the \bold{\code{snames}} argument.

If \bold{\code{input}} is an object of class \code{"qca"} (the result of the
function \bold{\code{\link[QCA]{minimize}()}} from package \bold{\pkg{QCA}}), a
factorization is performed for each of the minimized solutions.

}

\value{
A named list, each component containing all possible factorizations of the input
expression(s), found in the name(s).
}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press
}

\seealso{\code{\link{translate}}}

\examples{

# typical example with redundant conditions
factorize(a~b~cd + a~bc~d + a~bcd + abc~d)

# results presented in alphabetical order
factorize(~one*two*~four + ~one*three + three*~four)

# to preserve a certain order of the set names
factorize(~one*two*~four + ~one*three + three*~four,
          snames = c(one, two, three, four))

# using pos - products of sums
factorize(~a~c + ~ad + ~b~c + ~bd, pos = TRUE)

\dontrun{
# make sure the package QCA is loaded
library(QCA)

# using an object of class "qca" produced with function minimize()
# in package QCA

pCVF <- minimize(CVF, outcome = "PROTEST", incl.cut = 0.8,
                 include = "?", use.letters = TRUE)

factorize(pCVF)

# using an object of class "deMorgan" produced with negate()
factorize(negate(pCVF))
}

}

\keyword{functions}
