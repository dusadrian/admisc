`wholeNumeric` <- function(x, each = FALSE) {
    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        return(Recall(unclass(x), each = each))
    }

    result <- logical(length(x))
    isna <- is.na(x)
    result[isna] <- NA

    if (all(isna) || is.logical(x)) {
        if (each) {
            return(result)
        }
        return(FALSE)
    }
    
    x <- asNumeric(x[!isna])
    result[!isna] <- floor(x) == x

    if (each) {
        return(result)
    }
    
    return(all(result[!isna]))
}
