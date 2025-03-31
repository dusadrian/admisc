\name{About the admisc package}

\alias{admisc-package}

\docType{package}

\title{
Adrian Dusa's Miscellaneous
}

\description{
Contains functions used across packages 'DDIwR', 'QCA' and 'venn'.
Interprets and translates, factorizes and negates SOP - Sum of Products
expressions, for both binary and multi-value crisp sets, and extracts
information (set names, set values) from those expressions. Other functions
perform various checks if possibly numeric (even if all numbers reside in a
character vector) and coerce to numeric, or check if the numbers are whole. It
also offers, among many others, a highly versatile recoding routine and some
more flexible alternatives to the base functions \code{with()} and
\code{within()}.
SOP simplification functions in this package use related minimization from
package \bold{\pkg{QCA}}, which is recommended to be installed despite not being
listed in the Imports field, due to circular dependency issues.
}

\details{
    \tabular{ll}{
    Package: \tab admisc\cr
    Type:    \tab Package\cr
    Version: \tab 0.38\cr
    Date:    \tab 2025-03-30\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusa.adrian@unibuc.ro}

\bold{Maintainer}:\cr
Adrian Dusa
}


\keyword{package}
