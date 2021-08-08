`possibleNumeric` <- function(x) {
    if (all(is.na(x))) {
        return(FALSE)
    }

    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        num <- Recall(unclass(x))
        
        labels <- attr(x, "labels")
        if (!is.null(labels) && num) {
            # num <- sum(is.na(suppressWarnings(as.numeric(labels)))) == 0 # without na.omit() but why...?
            return(Recall(labels))
        }

        return(num)
    }

    if (is.numeric(x)) {
        return(TRUE)
    }

    if (is.factor(x)) {
        return(!any(is.na(suppressWarnings(as.numeric(levels(x))))))
    }
    
    if (any(grepl("[^!-~ ]", x))) {
        return(FALSE)
    }

    # as.character converts everything (especially factors)
    return(!any(is.na(suppressWarnings(as.numeric(na.omit(x))))))
}
