\name{Numeric testing and coercion}

\alias{asNumeric}
\alias{possibleNumeric}
\alias{wholeNumeric}

\title{Numeric vectors}

\description{
Coerces objects to class "numeric", and checks if an object is numeric.
}

\usage{
asNumeric(x)
possibleNumeric(x)
wholeNumeric(x)
}

\arguments{
  \item{x}{A vector of values}
}


\details{
Unlike the function \bold{\code{\link[base]{as.numeric}()}} from the \bold{\pkg{base}}
package, the function \bold{\code{asNumeric()}} coerces to numeric without a warning if
any values are not numeric. All such values are considered NA missing.

The function \bold{\code{possibleNumeric()}} tests if the values in a vector are possibly
numeric, irrespective of their storing as character or numbers.

Function \bold{\code{wholeNumeric()}} tests if numbers in a vector are whole (round) numbers.
Whole numbers are different from \dQuote{integer} numbers (which have special memory
representation), and consequently the function \bold{\code{\link[base]{is.integer}()}} tests
something different, how numbers are stored in memory (see the description of function
\bold{\code{\link[base]{double}()}} for more details).
}


\seealso{\code{\link[base]{numeric}}, \code{\link[base]{integer}}, \code{\link[base]{double}}} 


\author{
Adrian Dusa
}

\examples{
x <- c("-.1", " 2.7 ", "B")
asNumeric(x) # (-0.1, 2.7, NA)  and no warning

possibleNumeric(x) # FALSE

possibleNumeric(c("1", 2, 3)) # TRUE

is.integer(1) # FALSE

# Signaling an integer in R 
is.integer(1L) # TRUE

wholeNumeric(1) # TRUE
}


\keyword{functions}
