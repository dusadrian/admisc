permutations <- function(x) {
    if (length(x) == 1) {
        return(x)
    }
    
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
        res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }

    return(res)
}
