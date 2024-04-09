`possibleNumeric` <- function(x, each = FALSE) {

    result <- rep(NA, length(x))
    isna <- is.na(x)

    if (all(isna)) {
        if (each) {
            return(result)
        }

        return(FALSE)
    }

    if (is.logical(x)) {
        if (each) {
            result <- logical(length(x))
            result[isna] <- NA
            return(result)
        }

        return(FALSE)
    }

    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        num <- Recall(unclass(x), each = each)

        labels <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels) && !each && num) {
            return(Recall(labels))
        }

        return(num)
    }

    if (is.numeric(x)) {
        if (each) {
            result[!isna] <- TRUE
            return(result)
        }

        return(TRUE)
    }

    if (is.factor(x)) {
        x <- as.character(x)
    }
    x <- gsub("\u00a0", " ", x) # multibyte space

    multibyte <- grepl("[^!-~ ]", x)

    if (any(multibyte)) {
        isna[multibyte] <- TRUE
        result[multibyte] <- FALSE
        x[multibyte] <- NA
    }

    eachx <- suppressWarnings(as.numeric(na.omit(x)))
    result[!isna] <- !is.na(eachx)

    if (each) {
        return(result)
    }

    result <- result[!is.na(result)]
    if (length(result) == 0) {
        return(FALSE)
    }
    return(all(result))

    return(!any(is.na(suppressWarnings(as.numeric(na.omit(x))))))
}
