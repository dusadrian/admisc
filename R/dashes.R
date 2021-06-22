`dashes` <- function() {
    # integer raw vector
    irv <- c(45, 226, 128, 147)
    chrs <- rawToChar(as.raw(irv))

    if (any(grepl("[^!-~ ]", chrs))) {
        return("-")
    }

    paste(unlist(strsplit(chrs, split = "")), collapse = "|")
}
