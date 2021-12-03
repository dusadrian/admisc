`wholeNumeric` <- function(x, each = FALSE) {
    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        return(Recall(unclass(x), each = each))
    }

    result <- FALSE
    isna <- is.na(x)
    x <- asNumeric(x)

    if (each) {
        result <- rep(NA, length(x))
        result[!isna] <- FALSE

        if (any(!isna)) {
            x <- x[!isna]
            result[!isna & !is.na(x)] <- floor(x[!is.na(x)]) == x[!is.na(x)]
        }

        return(result)
    }
    else {
        if (all(is.na(x)) || any(!isna & is.na(x))) {
            return (FALSE)
        }
        
        return(all(floor(x) == x, na.rm = TRUE))
    }
}
