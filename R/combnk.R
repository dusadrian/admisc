#' Generate all combinations of n numbers, taken k at a time
#'
#' A fast function to generate all possible combinations of n numbers, taken k at a time,
#' starting from the first k numbers or starting from a combination that contain a
#' certain number.
#'
#' @name combnk
#' @rdname combnk
#' @rawRd
#' \usage{
#' combnk(n, k, ogte = 0, zerobased = FALSE)
#' }
#'
#' \arguments{
#'     \item{n}{Vector of any kind, or a numerical scalar.}
#'     \item{k}{Numeric scalar.}
#'     \item{ogte}{At least one value greater than or equal to this number.}
#'     \item{zerobased}{Logical, zero or one based.}
#' }
#'
#' \details{
#' When a scalar, argument \code{n} should be numeric, otherwise when a vector its
#' length should not be less than \code{k}.
#'
#' When the argument \bold{\code{ogte}} is specified, the combinations will sequentially
#' be incremented from those which contain a certain number, or a certain position from
#' \code{n} when specified as a vector.
#' }
#'
#'
#' \value{
#' A matrix with \code{k} rows and \code{choose(n, k)} columns.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' combnk(5, 2)
#'
#' combnk(5, 2, ogte = 3)
#'
#' combnk(letters[1:5], 2)
#' }
#'
#' \keyword{functions}
NULL
#' @export
`combnk` <- function(n, k, ogte = 0, zerobased = FALSE) {
    
    if (!is.numeric(k)) {
        stopError("Argument k should be numeric.")
    }
    
    if (length(k) != 1L) {
        stopError("Argument k should be a scalar of length 1.")
    }
    
    if (k < 0) {
        stopError("Argument k should be positive.")
    }

    len <- length(n)
    lngt1 <- len > 1

    if (lngt1) {
        if (len < k) {
            stopError("Argument k cannot be greater than the length of n.")
        }
    }
    else {
        if (!is.numeric(n)) {
            stopError("When scalar, argument n should be numeric.")
        }

        if (n < k) {
            stopError("Argument n should be greater than or equal to k.")
        }
    }

    copyn <- n
    if (lngt1) {
        n <- len
    }
    
    resmat <- .Call(
        "C_ombnk",
        list(
            n = as.integer(n),
            k = as.integer(k),
            ogte = as.integer(ogte),
            zerobased = as.integer(zerobased)
        ),
        PACKAGE = "admisc"
    )

    if (lngt1) {
        resmat <- matrix(copyn[resmat], nrow = nrow(resmat))
    }

    return(resmat)
}
