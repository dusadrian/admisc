#' Coerce an atomic vector to numeric or integer, if possible
#'
#' This function verifies if an R vector is possibly numeric, and further if the
#' numbers inside are whole numbers.
#'
#' @name coerceMode
#' @rdname coerceMode
#' @rawRd
#' \usage{
#' coerceMode(x)
#' }
#'
#' \arguments{
#'     \item{x}{An atomic R vector}
#' }
#'
#' \value{
#'     An R vector of coerced mode.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' obj <- c("1.0", 2:5)
#'
#' is.integer(coerceMode(obj))
#' }
#'
#' \keyword{functions}
NULL
#' @export
`coerceMode` <- function(x) {

    if (!is.atomic(x)) {
        stopError("The input is not atomic.")
    }

    if (
        !is.numeric(x) && 
        (possibleNumeric(x) || all(is.na(x)))
    ) {
        x <- asNumeric(x)
    }

    if (
        !is.integer(x) &&
        wholeNumeric(x) &&
        # some whole numbers might be too big to be represented in memory
        # as integers, in which case a warning will be captured
        # otherwise, if nothing is captured (the result is null) everything is ok
        is.null(tryCatchWEM(as.integer(x)))
    ) {
        x <- as.integer(x)
    }

    return(x)
}
