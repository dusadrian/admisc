`inside` <- function(data, expr, ...) {
    UseMethod("inside")
}

`inside.data.frame` <- function(data, expr, ...) {
    # modified version of within.data.frame
    dataname <- deparse(substitute(data))
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    l <- as.list(e, all.names = TRUE)
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    nl <- names(l)
    ## del: variables to *del*ete from data[]; keep non-NULL ones
    del <- setdiff(names(data), nl)
    data[nl] <- l
    data[del] <- NULL
    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        # for instance inside(obj$DF, dosomething)
        # where obj$DF is not an "object" to replace
        structure_string <- paste(capture.output(dput(data)), collapse = " ")

        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}



`inside.list` <- function(data, expr, keepAttrs = TRUE, ...) {
    # modified version of within.list
    parent <- parent.frame()
    dataname <- deparse(substitute(data))
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr", "keepAttrs"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    if (keepAttrs) { # names() kept in original order; also other attributes
        l <- as.list(e, all.names=TRUE)
        nl <- names(l)
        del <- setdiff(names(data), nl) # variables to delete
        data[nl] <- l
        data[del] <- NULL
    } else { # (order should not matter in *named* list)
        data <- as.list(e, all.names=TRUE)
    }

    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        structure_string <- paste(capture.output(dput(data)), collapse = " ")

        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}
