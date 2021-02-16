`obj.rda` <- function(.filename) {
    attach(.filename)
    attached_filename <- paste0("file:", .filename, "")
    nms <- ls(envir = as.environment(attached_filename))
    eval(substitute(detach(name), list(name = attached_filename)))
    return(nms)
}
