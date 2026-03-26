#' Extract information between quotes in a string
#'
#' Functions to extract the between the (escaped) quotes, in a string.
#'
#' @name betweenQuotes
#' @rdname betweenQuotes
#' @rawRd
#' \usage{
#' betweenQuotes(x)
#' }
#'
#' \arguments{
#'   \item{x}{A string.}
#' }
#'
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' x <- "An example of \"quoted\" text."
#'
#' betweenQuotes(x)
#' }
#'
#' \keyword{functions}
NULL
#' @export
`betweenQuotes` <- function(x) {
    pos <- gregexpr("\"", x)
    lpos <- length(pos[[1]])
    if (lpos == 0) {
        return("")
    }
    else if (lpos%%2 != 0) {
        stopError("Odd number of quotes")
    }
    else {
        pos <- pos[[1]]
        result <- character(lpos)
        for (i in seq(1, lpos, by = 2)) {
            result[i] <- substr(x, pos[i] + 1, pos[i + 1] - 1)
        }
        return(result[nchar(result) > 0])
    }
}
