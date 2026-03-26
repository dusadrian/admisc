# In principle, such functions are found in packages such as stringr or stringi
# but it is not worth adding dependencies just for these

#' @export
`padLeft` <- function(x, n) {
    paste(c(rep(" ", n), x), collapse = "", sep = "")
}

#' @export
`padRight` <- function(x, n) {
    paste(c(x, rep(" ", n)), collapse = "", sep = "")
}

#' @export
`padBoth` <- function(x, n) {
    n1 <- ceiling(n/2)
    n2 <- floor(n/2)
    paste(c(rep(" ", n1), x, rep(" ", n2)), collapse = "", sep = "")
}
