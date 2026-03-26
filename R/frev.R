#' Inverts the values of a factor
#'
#' Provides a reversed version of the values from a factor, for instance
#'   a Likert type response scale.
#'
#' @name frev
#' @rdname frev
#' @aliases finvert
#' @rawRd
#' \usage{
#' frev(x, labels = FALSE)
#' }
#'
#' \arguments{
#'   \item{x}{A factor}
#'   \item{labels}{Logical, invert the labels as well}
#' }
#'
#' \details{
#'   The argument \code{labels} can also be used for the levels of a factor.
#' }
#'
#' \value{A factor of the same length as the original one.}
#'
#' \author{Adrian Dusa}
#'
#' \examples{
#' words <- c("ini", "mini", "miny", "moe")
#' variable <- factor(words, labels = words)
#'
#' # inverts the values, preserving the labels' order
#' frev(variable)
#'
#' # inverts both values and labels
#' frev(variable, labels = TRUE)
#'
#' }
#'
#' \keyword{misc}
NULL
#' @export
`frev` <- function(x, labels = FALSE) {
    # to do, same for haven_labelled and declared
    if (!is.factor(x)) {
        stopError("The variable is not a factor.")
    }
    flist <- list(levels(x), rev(levels(x)))
    return(factor(x, levels = flist[[1 + !labels]], labels = flist[[1 + labels]]))
}

`finvert` <- function(...) {
    .Deprecated(msg = "Function finvert() is deprecated, use frev().\n")
    frev(...)
}
