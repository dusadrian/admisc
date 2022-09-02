`wholeNumeric` <- function(x, each = FALSE) {
    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        return(Recall(unclass(x), each = each))
    }

    if (!possibleNumeric(x) & !each) {
        return(FALSE)
    }

    result <- logical(length(x))
    isna <- is.na(x)
    result[isna] <- NA

    if (all(isna) || is.logical(x)) {
        # each is certainly TRUE because if they are all missing or all logical
        # it would not be numeric, which means the only condition continuing the
        # function on line 6 is each = TRUE otherwise line 7 would stop
        return(result)
    }
    
    x <- asNumeric(x)
    # some characters might be recoded to NA when coerced to numeric
    isnax <- is.na(x)

    result[!isna & isnax] <- FALSE
    isna <- isna | isnax
    x <- x[!isna]
    
    result[!isna] <- floor(x) == x

    if (each) {
        return(result)
    }
    
    return(all(result[!isna]))
}
