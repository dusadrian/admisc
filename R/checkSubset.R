`checkSubset` <- function(mat, implicants = TRUE) {
    for (i in 1:2) {
        eqz <- mat[i, ] == ifelse(implicants, 0, -1)
        if (nrow(unique(mat[, !eqz, drop = FALSE])) == 1) {
            return(3 - i)
        }
    }
    return(NULL)
}
