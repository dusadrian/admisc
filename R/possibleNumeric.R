`possibleNumeric` <- function(x, each = FALSE) {

    result <- rep(NA, length(x))
    nax <- is.na(x)

    if (all(nax)) {
        if (each) {
            return(result)
        }

        return(FALSE)
    }

    if (is.logical(x)) {
        if (each) {
            result <- logical(length(x))
            result[nax] <- NA
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
            result[!nax] <- TRUE
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
        result[multibyte] <- FALSE
    }

    if (sum(nax) < length(x)) {
        eachx <- suppressWarnings(as.numeric(x[!nax & !multibyte]))
        result[!nax & !multibyte] <- !is.na(eachx)
    }

    if (each | length(x) == 1) {
        return(result)
    }

    return(all(result[!nax]))
}
