`classify` <- function(x, class = "admisc_simplify") {
    class(x) <- c("character", class)
    return(x)
}
