`betweenQuotes` <- function(x) {
    pos <- gregexpr("\"", x)
    lpos <- length(pos[[1]])
    if (lpos == 0) {
        return("")
    }
    else if (lpos%%2 != 0) {
        stopError("Odd number of quotes")
    }
    else {
        pos <- pos[[1]]
        result <- character(lpos)
        for (i in seq(1, lpos, by = 2)) {
            result[i] <- substr(x, pos[i] + 1, pos[i + 1] - 1)
        }
        return(result[nchar(result) > 0])
    }
}