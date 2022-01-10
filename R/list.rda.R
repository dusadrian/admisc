`list.rda` <- function(.filename) {
    load(.filename)
    return(as.list(environment()))
}

`list.rdata` <- function(...) {
    list.rda(...)
}
