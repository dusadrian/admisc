`finvert` <- function(x, levels = FALSE) {
    # to do, same for haven_labelled and declared
    if (!is.factor(x)) {
        stopError("The variable is not a factor.")
    }
    flist <- list(levels(x), rev(levels(x)))
    return(factor(x, levels = flist[[1 + !levels]], labels = flist[[1 + levels]]))
}
