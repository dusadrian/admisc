`tagged_na_value` <- function(x) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }
    .Call("C_is_tagged_na", x, PACKAGE = "admisc")
}

`get_na_tag` <- function(x) {
    .Call("C_na_tag", x, PACKAGE = "admisc")
}

`make_tagged_na` <- function(x) {
    .Call("C_tagged_na", x, PACKAGE = "admisc")
}


# from a string, such as ".a" or "NA(a)"
`tag_from_string` <- function(x) {
    return(gsub("[A-Z]|\\(|\\)|\\.", "",  x))
}

# is a string representing a tagged NA such as ".a" or "NA(a)"?
`tagged_string` <- function(x) {
    x <- as.character(x)
    return(is.element(tag_from_string(x), letters) & 
        ((nchar(x) == 2 & grepl("^\\.", x)) | (nchar(x) == 5 & grepl("^NA\\(", x)))
    )
}
