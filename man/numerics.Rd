\name{Numeric testing and coercion}

\alias{asNumeric}
\alias{possibleNumeric}
\alias{wholeNumeric}

\title{Numeric vectors}

\description{
Coerces objects to class "numeric", and checks if an object is numeric.
}

\usage{
asNumeric(x, ...)
possibleNumeric(x, each = FALSE)
wholeNumeric(x, each = FALSE)
}

\arguments{
  \item{x}{A vector of values}
  \item{each}{Logical, return the result for each value in the vector}
  \item{...}{Other arguments to be passed for class based methods}
}


\details{
Unlike the function \bold{\code{as.numeric}()} from the \bold{\pkg{base}}
package, the function \bold{\code{asNumeric()}} coerces to numeric without a
warning if any values are not numeric. All such values are considered NA missing.

This is a generic function, with specific class methods for factors and objects
of class \dQuote{declared}. The usual way of coercing factors to numeric is
meaningless, converting the inner storage numbers. The class method of this
particular function coerces the levels to numeric, via the default activated
argument \code{levels}.

For objects of class \dQuote{declared}, a similar argument called \code{na_values}
is by default activated to coerce the declared missing values to numeric.

The function \bold{\code{possibleNumeric()}} tests if the values in a vector are
possibly numeric, irrespective of their storing as character or numbers. In the
case of factors, it tests its levels representation.

Function \bold{\code{wholeNumeric()}} tests if numbers in a vector are whole
(round) numbers. Whole numbers are different from \dQuote{integer} numbers (which
have special memory representation), and consequently the function
\bold{\code{is.integer}()} tests something different, how numbers are stored in
memory (see the description of function \bold{\code{\link[base]{double}()}} for
more details).

The function 
}


\seealso{
  \code{\link[base]{numeric}},
  \code{\link[base]{integer}},
  \code{\link[base]{double}}
} 


\author{
Adrian Dusa
}

\examples{
x <- c("-.1", " 2.7 ", "B")
asNumeric(x) # no warning

f <- factor(c(3, 2, "a"))

asNumeric(f)

asNumeric(f, levels = FALSE)

possibleNumeric(x) # FALSE

possibleNumeric(x, each = TRUE) # TRUE  TRUE FALSE

possibleNumeric(c("1", 2, 3)) # TRUE

is.integer(1) # FALSE

# Signaling an integer in R 
is.integer(1L) # TRUE

wholeNumeric(1) # TRUE

wholeNumeric(c(1, 1.1), each = TRUE) # TRUE FALSE
}


\keyword{functions}
