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
