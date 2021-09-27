`asSOP` <- function(
    expression = "", snames = "", noflevels = NULL
) {
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    # dots <- list(...)
    
    arglist <- list(snames = snames)

    if (!is.null(noflevels)) {
        arglist$noflevels <- noflevels
    }

    return(
        unname(sapply(expression, function(x) {
            if (grepl("[(|)]", x)) {
                x <- do.call(
                    expandBrackets,
                    c(list(expression = x), arglist)
                )
            }
            return(x)
        }))
    )
}
