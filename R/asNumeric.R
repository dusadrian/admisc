`asNumeric` <- function(x) {
    if (is.numeric(x)) {
        return(x)
    }

    if (is.factor(x)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }
    
    return(suppressWarnings(as.numeric(as.character(x))))
}
