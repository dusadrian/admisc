# `tag` <- function(x) {
#     .Call("C_na_tag", x, PACKAGE = "admisc")
# }

# `tag<-` <- function(x, value = NULL) {
#     checkTag(value)
#     x <- rep(.Call("C_tagged_na", value, PACKAGE = "admisc"), length(x))
# }



`tag_na` <- function(...) {
    return(.Call("_tag_na", as.character(c(...)), PACKAGE = "admisc"))
}

`has_tag` <- function(x, tag = NULL) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }

    if (!is.null(tag) && !is.atomic(tag) && (length(tag) > 1 || is.na(tag))) {
        cat("\n")
        stop("`tag` should be a vector of length 1.\n\n", call. = FALSE)
    }
    
    if (!is.null(tag)) {
        tag <- as.character(tag)
    }

    return(.Call("_has_tag", x, tag, PACKAGE = "admisc"))
}

`get_tag` <- function(x) {
    if (is.character(x)) {
        return(gsub("N|A|\\(|\\)|\\.", "",  x))
    }
    else if (is.double(x)) {
        x <- .Call("_get_tag", x, PACKAGE = "admisc")
        if (!any(is.na(suppressWarnings(as.numeric(na.omit(x)))))) {
            x <- as.numeric(x)
        }
        return(x)
    }
    else {
        # cat("\n")
        # stop("Unsuitable input to extract a tagged value.\n\n", call. = FALSE)
        return(rep(NA, length(x)))
    }
}

# is a string representing a tagged NA such as ".a" or "NA(a)"?
`is_tagged_string` <- function(x) {
    if (!is.character(x)) {
        return(logical(length(x)))
    }

    ncharx <- nchar(x)

    return(
        ncharx > 1 &
        ((ncharx < 4 & grepl("^\\.", x)) | (ncharx < 7 & grepl("^NA\\(", x) & grepl("\\)", x)))
    )
}
