`dashes` <- function() {
    # integer raw vector
    irv <- c(45, 226, 128, 147)
    paste(unlist(strsplit(rawToChar(as.raw(irv)), split = "")), collapse = "|")
}
