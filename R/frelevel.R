`frelevel` <- function(variable, levels) {
    # to do: the same with havel_labelled
    if (!is.factor(variable)) {
        stopError("The input variable is not a factor.")
    }
    
    if (any(!(levels %in% levels(variable)))) {
        stopError("One or more levels do not exist in the input variable.")
    }
    
    for (i in seq_len(length(levels))) {
        variable <- relevel(variable, ref = rev(levels)[i])
    }
    
    return(variable)
}
