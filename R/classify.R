`classify` <- function(x, class = "admisc_simplify") {
    attrx <- attributes(x)
    attrx$class <- c("character", class)
    attributes(x) <- attrx
    return(x)
}
