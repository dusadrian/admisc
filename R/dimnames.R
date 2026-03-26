#' Set matrix row or column names
#'
#' Set matrix row or column names without copying, especially useful for (very)
#' large matrices.
#'
#' @name setColnames
#' @rdname dimnames
#' @aliases dimnames
#' @aliases setRownames
#' @aliases setDimnames
#' @rawRd
#' \usage{
#' setColnames(matrix, colnames)
#' setRownames(matrix, rownames)
#' setDimnames(matrix, nameslist)
#' }
#'
#' \arguments{
#'   \item{matrix}{An R matrix}
#'   \item{colnames}{Character vector of column names}
#'   \item{rownames}{Character vector of row names}
#'   \item{nameslist}{A two-component list containing rownames and colnames}
#' }
#'
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#'
#' mat <- matrix(1:9, nrow = 3)
#' setDimnames(mat, list(LETTERS[1:3], letters[1:3]))
#' }
#'
#'
#' \keyword{functions}
NULL
#' @export
`setColnames` <- function(matrix, colnames) {
    invisible(.Call("C_setColnames", matrix, colnames))
}

#' @export
`setRownames` <- function(matrix, rownames) {
    invisible(.Call("C_setRownames", matrix, rownames))
}

#' @export
`setDimnames` <- function(matrix, nameslist) {
    invisible(.Call("C_setDimnames", matrix, nameslist))
}
