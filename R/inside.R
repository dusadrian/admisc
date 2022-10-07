`inside` <- function(data, expr, ...) {
    UseMethod("inside")
}

`inside.data.frame` <- function(data, expr, ...) {
    # modified version of within.data.frame
    dataname <- deparse(substitute(data))
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    l <- as.list(e, all.names = TRUE)
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    nl <- names(l)
    ## del: variables to *del*ete from data[]; keep non-NULL ones
    del <- setdiff(names(data), nl)
    data[nl] <- l
    data[del] <- NULL
    parent[[dataname]] <- data
}
