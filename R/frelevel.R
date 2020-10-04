`frelevel` <- function(variable, levels) {
    # to do: the same with havel_labelled
    if (!is.factor(variable)) {
        cat("\n")
        stop("The input variable is not a factor.\n\n", call. = FALSE)
    }
    
    if (any(!(levels %in% levels(variable)))) {
        cat("\n")
        stop("One or more levels do not exist in the input variable.\n\n", call. = FALSE)
    }
    
    for (i in seq_len(length(levels))) {
        variable <- relevel(variable, ref = rev(levels)[i])
    }
    
    return(variable)
}
