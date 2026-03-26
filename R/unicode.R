#' @export
`dashes` <- function() {
    return(c("\u002d", "\u2013"))
}

#' @export
`tildae` <- function() {
    return(c("\u007e", "\u223c", "\u00ac", "\u223d"))
}

#' @export
`singlequotes` <- function() {
    return(c("\u00b4", "\u0060", "\u2018", "\u2019"))
}

#' @export
`doublequotes` <- function() {
    return(c("\u201c", "\u201d"))
}

#' @export
`spaces` <- function() {
    return("\u00a0") # multibyte space
}

