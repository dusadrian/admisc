`asNumeric` <- function(x) {
    if (is.numeric(x)) {
        return(x)
    }

    if (is.factor(x)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }

    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~]", x)
    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))
    return(result)
}
