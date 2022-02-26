`listRDA` <- function(.filename) {
    load(.filename)
    return(as.list(environment()))
}
