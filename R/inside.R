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



`inside.list` <- function(data, expr, keepAttrs = TRUE, ...) {
    # modified version of within.list
    parent <- parent.frame()
    dataname <- deparse(substitute(data))
    e <- evalq(environment(), data, parent)
    eval(substitute(expr), e)
    if (keepAttrs) { # names() kept in original order; also other attributes
        l <- as.list(e, all.names=TRUE)
        nl <- names(l)
        del <- setdiff(names(data), nl) # variables to delete
        data[nl] <- l
        data[del] <- NULL
        parent[[dataname]] <- data
    } else { # (order should not matter in *named* list)
	    parent[[dataname]] <- as.list(e, all.names=TRUE)
    }
}
