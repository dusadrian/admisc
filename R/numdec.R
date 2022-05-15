`numdec` <- function(x, each = FALSE, na.rm = TRUE, maxdec = 15) {

    pN <- possibleNumeric(x, each = TRUE)

    # sum(pN), maybe each = TRUE and it's a vector
    if (sum(na.omit(pN)) == 0) {
        stopError("'x' should contain at least one (possibly) numeric value.")
    }

    result <- rep(0, length(x))
    x <- asNumeric(x)
    attributes(x) <- NULL
    result[is.na(x)] <- NA
    hasdec <- agtb(x %% 1, 0)

    if (any(hasdec, na.rm = TRUE)) {
        wdec <- which(hasdec)
        x[wdec] <- abs(x[wdec])
        x[wdec] <- trimstr(formatC(x[wdec] - floor(x[wdec]), digits = maxdec))
        result[wdec] <- nchar(asNumeric(gsub("^0.", "", x[wdec])))
    }

    if (each) {
        return(result)
    }

    return(max(result, na.rm = na.rm))
}
