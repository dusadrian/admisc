`coerceMode` <- function(x) {

    if (!is.atomic(x)) {
        stopError("The input is not atomic.")
    }

    if (possibleNumeric(x) || all(is.na(x))) {
        if (wholeNumeric(x) && is.null(tryCatchWEM(as.integer(x)))) {
            x <- as.integer(x)
        }
        else {
            x <- asNumeric(x)
        }
    }

    return(x)
}
