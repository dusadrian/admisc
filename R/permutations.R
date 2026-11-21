#' Calculates the permutations of a vector
#'
#' Generates all possible permutations of elements from a vector.
#'
#' @name permutations
#' @rdname permutations
#' @rawRd
#' \usage{
#' permutations(x)
#' }
#'
#' \arguments{
#'     \item{x}{Any kind of vector.}
#' }
#'
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#'
#' permutations(1:3)
#'
#' }
#'
#' \keyword{functions}
NULL
#' @export
permutations <- function(x) {
    if (length(x) == 1) {
        return(x)
    }
    
    n <- length(x)
    if (!n) return(matrix(nrow = 0L, ncol = 0L))

    # Each leading element uses the same permutation of the remaining positions.
    positions <- matrix(1L, 1L, 1L)
    for (size in 2:n) {
        block <- nrow(positions)
        nextmat <- matrix(0L, size * as.double(block), size)
        rows <- seq_len(block)
        for (first in seq_len(size)) {
            nextmat[rows, 1L] <- first
            nextmat[rows, -1L] <- positions + (positions >= first)
            if (first < size) rows <- rows + block
        }
        positions <- nextmat
    }
    values <- if (is.factor(x)) as.integer(x) else if (is.raw(x)) as.logical(x) else x
    result <- matrix(values[positions], ncol = n)
    if (!is.null(names(x))) rownames(result) <- names(x)[positions[, n - 1L]]
    result
}
