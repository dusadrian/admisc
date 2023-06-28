`tildae` <- function() {
    return(c("\u007e", "\u223c", "\u00ac", "\u223d"))
}

`tilde1st` <- function(x) {
    is.element(
        substring(
            gsub(
                paste0("[[:space:]|", "\u00a0", "]"), # multibyte space
                "",
                x
            ),
            1, 1
        ),
        tildae()
    )
}

`hastilde` <- function(x) {
    grepl(paste(tildae(), collapse = "|"), x)
}

`notilde` <- function(x) {
    gsub(
        paste(tildae(), collapse = "|"),
        "",
        gsub(
            paste0("[[:space:]|", "\u00a0", "]"), # multibyte space
            "",
            x
        )
    )
}
