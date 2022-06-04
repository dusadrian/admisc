`tildae` <- function() {
    # integer raw vector, obtained with e.g.: as.integer(charToRaw("~"))
    irv <- c(126, 226, 136, 188, 194, 172, 226, 136, 189)
    chrs <- rawToChar(as.raw(irv))
    
    # if (any(grepl("[^!-~ ]", chrs))) {
    #     # ????? this is always TRUE !!
    #     return("~")
    # }
    
    unlist(strsplit(chrs, split = ""))
}

irv <- c(194, 160)
multibyte_space <- rawToChar(as.raw(irv))

`tilde1st` <- function(x) {
    is.element(
        substring(
            gsub(
                paste0("[[:space:]|", multibyte_space, "]"),
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
            paste0("[[:space:]|", multibyte_space, "]"),
            "",
            x
        )
    )
}
