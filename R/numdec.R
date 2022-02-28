`numdec` <- function(x, each = FALSE, na.rm = TRUE) {

    pN <- possibleNumeric(x, each = each)

    if ((each & sum(pN) == 0) || (!each & !pN)) {
        stopError("'x' values should be numeric.")
    }

    result <- rep(NA, length(x))
    x <- asNumeric(x)
    hasdec <- agtb(x %% 1, 0)

    if (any(hasdec, na.rm = TRUE)) {
        if (each) {
            for (i in seq(length(x))) {
                if (pN[i]) {
                    result[i] <- 0
                    if (hasdec[i]) {
                        xi <- format(x[i], scientific = FALSE)
                        result[i] <- nchar(unlist(strsplit(xi, split = "[.]"))[2])
                    }
                }
            }

            return(result)
        }

        if (na.rm) {
            x <- na.omit(x)
        }

        if (any(is.na(x))) {
            return(NA)
        }
        
        x <- format(x, scientific = FALSE)
        return(nchar(unlist(strsplit(x, split = "[.]"))[2]))
    }

    if (each) {
        result[pN] <- 0
        return(result)
    }

    if ((each & sum(pN) == length(x)) || pN) {
        return(0)
    }

    return(NA)
}
