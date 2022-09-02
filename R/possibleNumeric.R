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

    irv <- c(194, 160)
    multibyte_space <- rawToChar(as.raw(irv))
    x <- gsub(multibyte_space, " ", x)

    multibyte <- grepl("[^!-~ ]", x)
    if (any(multibyte)) {
        isna[multibyte] <- TRUE
        result[multibyte] <- FALSE
        x[multibyte] <- NA
    }

    if (each) {
        x <- suppressWarnings(as.numeric(na.omit(x)))
        result[!isna] <- !is.na(x)
        return(result)
    }

    return(!any(is.na(suppressWarnings(as.numeric(na.omit(x))))))
}
