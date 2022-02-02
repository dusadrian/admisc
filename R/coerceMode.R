`coerceMode` <- function(x) {

    if (!is.atomic(x)) {
        stopError("The input is not atomic.")
    }

    if (possibleNumeric(x) || all(is.na(x))) {
        if (wholeNumeric(x)) {
            testx <- tryCatchWEM(as.integer(x))
            if (is.element("warning", names(testx))) {
                x <- asNumeric(x)
            }
            else {
                x <- testx
            }
        }
        else {
            x <- asNumeric(x)
        }
    }

    return(x)
}
