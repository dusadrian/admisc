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
    
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
        res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }

    return(res)
}
