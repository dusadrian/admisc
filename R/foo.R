foo <- function(x) {
    return(parse(text = paste(unlist(lapply(sys.calls(), deparse)), collapse = "\n")))
}
