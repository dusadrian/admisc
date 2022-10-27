\name{replaceText}

\alias{replaceText}

\title{Replace text in a string}

\description{
Provides an improved method to replace strings, compared to function
\bold{\code{gsub}()} in package \bold{\pkg{base}}.
}

\usage{
replaceText(
    expression = "", target = "", replacement = "", protect = "",
    boolean = FALSE, ...)
}

\arguments{
  \item{expression}{Character string, usually a SOP - sum of products expression.}
  \item{target}{Character vector or a string containing the text to be replaced.}
  \item{replacement}{Character vector or a string containing the text to replace with.}
  \item{protect}{Character vector or a string containing the text to protect.}
  \item{boolean}{Treat characters in a boolean way, using upper and lower case letters.}
  \item{...}{Other arguments, from and to other functions.}
}

\details{
If the input expression is "J*JSR", and the task is to replace "J" with "A" and "JSR" with
"B", function \bold{\code{gsub}()} is not very useful since the letter "J" is
found in multiple places, including the second target.

This function finds the exact location(s) of each target in the input string, starting with
those having the largest number of characters, making sure the locations are unique. For
instance, the target "JSR" is found on the location from 3 to 5, while the target "J" is
is found on two locations 1 and 3, but 3 was already identified in the previously found
location for the larger target.

In addition, this function can also deal with target strings containing spaces.
}


\value{
The original string, replacing the target text with its replacement.
}

\author{
Adrian Dusa
}


\examples{
replaceText("J*JSR", "J, JSR", "A, B")

# same output, on input expresions containing spaces
replaceText("J*JS R", "J, JS R", "A, B")

# works even with Boolean expressions, where lower case
# letters signal the absence of the causal condition
replaceText("DEV + urb*LIT", "DEV, URB, LIT", "A, B, C", boolean = TRUE)
}

\keyword{functions}
