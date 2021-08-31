`asNumeric` <- function(x) {
    if (is.numeric(x)) {
        return(x)
    }

    if (is.factor(x)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }

    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~ ]", x)

    if (inherits(x, "haven_labelled")) {
        attributes(x) <- NULL
    }
    
    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))
    
    return(result)
}
