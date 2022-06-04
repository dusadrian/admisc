`makeTag` <- function(...) {
    x <- as.character(c(...))
    
    x <- .Call("_tag", x, PACKAGE = "admisc")
    class(x) <- "double"

    return(x)
}

`hasTag` <- function(x, tag = NULL) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }

    if (!is.null(tag) && !is.atomic(tag) && (length(tag) > 1 || is.na(tag))) {
        stopError("`tag` should be a vector of length 1.")
    }
    
    if (!is.null(tag)) {
        tag <- as.character(tag)
    }

    return(.Call("_has_tag", x, tag, PACKAGE = "admisc"))
}

`getTag` <- function(x) {
    if (is.double(x)) {
        x <- .Call("_get_tag", x, PACKAGE = "admisc")
        if (!any(is.na(suppressWarnings(as.numeric(na.omit(x)))))) {
            x <- as.numeric(x)
        }
        return(x)
    }
    else {
        # stopError("Unsuitable input to extract a tagged value.")
        return(rep(NA, length(x)))
    }
}

`anyTagged` <- function(x) {
    if (is.data.frame(x)) {
        i <- 1
        tagged <- FALSE
        while(!tagged & i <= ncol(x)) {
            tagged <- Recall(x[[i]])
            i <- i + 1
        }
        return(tagged)
    }

    if (is.double(x)) {
        return(.Call("_any_tagged", x, PACKAGE = "admisc"))
    }
    
    return(FALSE)
}
