`numdec` <- function(x, each = FALSE, na.rm = TRUE, maxdec = 15) {

    maxdec <- min(15, maxdec)

    pN <- possibleNumeric(x, each = TRUE)

    # sum(pN), maybe each = TRUE and it's a vector
    if (sum(na.omit(pN)) == 0) {
        stopError("'x' should contain at least one (possibly) numeric value.")
    }

    # asNumeric is important here because the (possible) number might arrive
    # as character through coercion, for instance c("A", 1e-04)
    if (is.character(x)) {
        x <- asNumeric(x)
    }

    result <- rep(NA, length(x))
    wpN <- which(pN)

    x <- abs(x[wpN])
    x <- x - floor(x) # all numbers are now 0.something

    x <- sub("0\\.", "",
        sub("0+$", "", # erase trailing zeros
            format(x, scientific = FALSE, digits = max(7, maxdec))
        )
    )

    if (any(w9 <- grepl("999999", x))) {
        # A floating point number like 234.1 might have been represented as
        # 0.0999999999999943 (after subtracting the floor)
        x[w9] <- sub(
            "0+", "1", # last 0 becomes 1
            sub("(*)999999.*", "\\1", x[w9]) # retains everthing <up to> the sequence
        )
    }

    if (any(w0 <- grepl("000000", x))) {
        # ex. 0.00000000000000001, for all practical purposes this is equal to 0
        x[w0] <- sub("(*)000000.*", "\\1", x[w0])
    }


    result[wpN] <- nchar(x)
    
    if (each) {
        return(pmin(result, maxdec))
    }

    return(min(maxdec, max(result, na.rm = na.rm)))
}


# numdec(c(1.1,-8.5,-5,145,5,10.15,pi,44532456.345243627,0), each = TRUE)
# numdec(c(234.1, 3.7500, 1.345, 3e-12), each = TRUE)
# numdec(c(234.1, 3.7500, 1.345, 3e-17), maxdec = 17)
# numdec(c(234.1, 3.7500, 1.345, 3e-17), maxdec = 3)
# numdec(123456.123456789) # !!
# numdec(c(2.45496e-5, 3e-17, 5.002e-5, 0.3, 123456789.123456789), each = TRUE) # !!



    # old way from DDIwR::getDecimals()
    # unique(
    #     nchar(
    #         format(abs(number), scientific = FALSE)
    #     ) -
    #     (trunc(log10(max(1, trunc(abs(number))))) + 1) -
    #     1
    # )
