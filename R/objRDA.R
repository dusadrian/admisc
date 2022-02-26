`objRDA` <- function(.filename) {
    attached_filename <- paste0("file:", .filename, "")
    suppressMessages(do.call("attach", list(what = .filename, name = attached_filename)))
    on.exit(eval(substitute(detach(name), list(name = attached_filename))))
    return(ls(envir = as.environment(attached_filename)))
}
