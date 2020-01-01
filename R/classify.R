`classify` <- function(x, class = "simplify") {
    class(x) <- c("character", class)
    return(x)
}
