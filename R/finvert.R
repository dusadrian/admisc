`finvert` <- function(x, levels = FALSE) {
    # to do, same for haven_labelled
    if (!is.factor(x)) {
        cat("\n")
        stop("The variable is not a factor.\n\n", call. = FALSE)
    }
    flist <- list(levels(x), rev(levels(x)))
    return(factor(x, levels = flist[[1 + !levels]], labels = flist[[1 + levels]]))
}

