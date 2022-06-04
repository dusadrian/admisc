`asNumeric` <- function(x, ...) {
    UseMethod("asNumeric")
}

`asNumeric.declared` <- function(x, ..., na_values = TRUE) {
    na_index <- attr(x, "na_index")
    attributes(x) <- NULL

    if (isTRUE(na_values)) {
        if (!is.null(na_index)) {
            # if the names are not numeric that is not a problem, NA is fine
            x[na_index] <- as.numeric(names(na_index))
        }
    }

    NextMethod()
}

`asNumeric.factor` <- function(x, ..., levels = TRUE) {
    if (isTRUE(levels)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }
    
    return(as.numeric(x))
}

`asNumeric.default` <- function(x, ...) {

    attributes(x) <- NULL

    if (is.numeric(x)) {
        return(x)
    }

    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~ ]", x)

    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))
    
    return(result)
}
