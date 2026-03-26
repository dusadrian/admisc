#' Modified \code{relevel()} function
#'
#' The base function \code{relevel()} accepts a single argument "ref", which
#'   can only be a scalar and not a vector of values. \code{frelevel()} accepts
#'   more (even all) levels and reorders them.
#'
#' @name frelevel
#' @rdname frelevel
#' @rawRd
#' \usage{
#' frelevel(variable, levels)
#' }
#'
#' \arguments{
#'   \item{variable}{The categorical variable of interest}
#'   \item{levels}{One or more levels of the factor, in the desired order}
#' }
#'
#' \value{A factor of the same length as the initial one.}
#'
#' \author{Adrian Dusa}
#'
#' \seealso{\code{\link[stats]{relevel}}}
#'
#' \examples{
#' words <- c("ini", "mini", "miny", "moe")
#' variable <- factor(words, levels = words)
#'
#' # modify the order of the levels, keeping the order of the values
#' frelevel(variable, c("moe", "ini", "miny", "mini"))
#'
#' }
#'
#' \keyword{functions}
NULL
#' @export
`frelevel` <- function(variable, levels) {
    # to do: the same with havel_labelled
    if (!is.factor(variable)) {
        stopError("The input variable is not a factor.")
    }
    
    if (any(!(levels %in% levels(variable)))) {
        stopError("One or more levels do not exist in the input variable.")
    }
    
    for (i in seq_len(length(levels))) {
        variable <- relevel(variable, ref = rev(levels)[i])
    }
    
    return(variable)
}
