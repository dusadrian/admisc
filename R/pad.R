# In principle, such functions are found in packages such as stringr or stringi
# but it is not worth adding dependencies just for these

`padLeft` <- function(x, n) {
    paste(c(rep(" ", n), x), collapse = "", sep = "")
}

`padRight` <- function(x, n) {
    paste(c(x, rep(" ", n)), collapse = "", sep = "")
}

`padBoth` <- function(x, n) {
    n1 <- ceiling(n/2)
    n2 <- floor(n/2)
    paste(c(rep(" ", n1), x, rep(" ", n2)), collapse = "", sep = "")
}
