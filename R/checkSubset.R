#' @export
`checkSubset` <- function(mat, implicants = TRUE) {
    for (i in 1:2) {
        eqz <- mat[i, ] == ifelse(implicants, 0, -1)
        if (nrow(unique(mat[, !eqz, drop = FALSE])) == 1) {
            return(3 - i)
        }
    }
    return(NULL)
}

# Process shorter terms first so every strict subset is seen before its supersets.
removeRedundantTerms <- function(mat, wildcard) {
    if (nrow(mat) < 2L) return(mat)
    width <- rowSums(mat != wildcard)
    redundant <- duplicated(mat) & width > 0L
    for (size in sort(unique(width[width > 0L & width < max(width)]))) {
        candidates <- which(width > size & !redundant)
        for (i in which(width == size & !redundant)) {
            if (!length(candidates)) break
            matches <- rep(TRUE, length(candidates))
            for (j in which(mat[i, ] != wildcard)) {
                matches <- matches & mat[candidates, j] == mat[i, j]
            }
            redundant[candidates[matches]] <- TRUE
            candidates <- candidates[!matches]
        }
    }
    mat[!redundant, , drop = FALSE]
}
