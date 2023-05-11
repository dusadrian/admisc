`undeclareit` <- function(x, drop = FALSE, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    
    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
    if (!is.null(na_index)) {
        # x <- ifelse(!is.na(missingValues), missingValues, x)
        x[na_index] <- names(na_index)
    }
    
    x <- coerceMode(x)
    
    attrx$na_index <- NULL
    attrx$na_values <- NULL
    attrx$na_range <- NULL

    if (isFALSE(drop)) {
        attributes (x) <- attrx
    }

    return(x)
}

`agtb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- (a - tol) > b
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

`altb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- a < (b - tol)
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

`agteb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- (a + tol) > b
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

`alteb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- a < (b + tol)
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

`aeqb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- abs(a - b) < tol
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

`aneqb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- abs(a - b) > tol
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
