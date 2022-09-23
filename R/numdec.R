`numdec` <- function(x, each = FALSE, na.rm = TRUE, maxdec = 15) {

    scipen <- options("scipen")$scipen
    options(scipen = 999) # for scientific numbers such as 1e-04

    on.exit(options(scipen = scipen))

    pN <- possibleNumeric(x, each = TRUE)

    # sum(pN), maybe each = TRUE and it's a vector
    if (sum(na.omit(pN)) == 0) {
        stopError("'x' should contain at least one (possibly) numeric value.")
    }

    result <- rep(NA, length(x))
    wpN <- which(pN)
    
    # asNumeric is important here because the (possible) number might arrive
    # as character through coercion, for instance c("A", 1e-04)
    x <- as.character(asNumeric(x[wpN]))

    x <- sapply(strsplit(x, split = "\\."), function(x) x[2])
    result[wpN] <- ifelse(is.na(x), 0, nchar(x))
    
    if (each) {
        return(result)
    }

    return(max(result, na.rm = na.rm))
}
