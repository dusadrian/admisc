# `tag` <- function(x) {
#     .Call("C_na_tag", x, PACKAGE = "admisc")
# }

# `tag<-` <- function(x, value = NULL) {
#     checkTag(value)
#     x <- rep(.Call("C_tagged_na", value, PACKAGE = "admisc"), length(x))
# }


`has_tag` <- function(x, tag = NULL) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }

    if (!is.null(tag) && (length(tag) > 1 || !is.character(tag) || is.na(tag))) {
        cat("\n")
        stop("The tag should be a character vector of length 1.\n\n", call. = FALSE)
    }

    .Call("C_has_tagged_na", x, tag, PACKAGE = "admisc")
}

`get_tag` <- function(x) {
    if (is.character(x)) {
        return(gsub("N|A|\\(|\\)|\\.", "",  x))
    }
    else if (is.double(x)) {
        return(.Call("C_na_tag", x, PACKAGE = "admisc"))
    }
    else {
        # cat("\n")
        # stop("Unsuitable input to extract a tagged value.\n\n", call. = FALSE)
        return(rep(NA, length(x)))
    }
}

`make_tagged_na` <- function(tag) {
    if (length(tag) > 1 || !is.character(tag) || is.na(tag)) {
        cat("\n")
        stop("The tag should be a character vector of length 1.\n\n", call. = FALSE)
    }
    .Call("C_tagged_na", tag, PACKAGE = "admisc")
}

# is a string representing a tagged NA such as ".a" or "NA(a)"?
`is_tagged_string` <- function(x) {
    x <- as.character(x)
    return(
        is.element(get_tag(x), letters) && 
        (
            (nchar(x) == 2 & grepl("^\\.", x)) ||
            (nchar(x) == 5 & grepl("^NA\\(", x) & grepl("\\)", x))
        )
    )

}
