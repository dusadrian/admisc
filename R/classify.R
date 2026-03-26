#' @export
classify <- function(x, class = "admisc_simplify") {
    class(x) <- unique(c(class, class(x)))
    x
}

