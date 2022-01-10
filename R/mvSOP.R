`mvSOP` <- function(
    expression = "", snames = "", data = NULL, keep.tilde = TRUE, ...
) {
    
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    dots <- list(...)

    if (any(grepl("\\[|\\]|\\{|\\}", expression))) {
        stopError("The expression is already in multi-value notation.")
    }

    if (identical(snames, "")) {
        if (!is.null(data)) {
            snames <- colnames(data)
        }
    }
    else {
        snames <- splitstr(snames)
    }

    noflevels <- NULL

    oldc <- newc <- c()

    categories <- list()
    if (is.null(data)) {
        if (!is.null(dots$categories)) {
            categories <- dots$categories
        }
    }
    else {
        infodata <- getInfo(data)
        noflevels <- infodata$noflevels
        categories <- infodata$categories
    }

    checkValid(
        expression = expression,
        snames = snames,
        data = data,
        categories = categories
    )

    if (length(categories) > 0) {
        fnames <- names(categories)
        # snames <- setdiff(snames, fnames)
        oldc <- c(paste0("~", fnames), fnames)
        newc <- c(paste0(fnames, "[0]"), paste0(fnames, "[1]"))

        for (i in seq(length(categories))) {
            values <- seq(length(categories[[i]])) - 1
            oldc <- c(oldc, categories[[i]])
            newc <- c(newc, paste0(fnames[i], "[", values, "]"))
            
            if (!keep.tilde) {
                oldc <- c(oldc, paste0("~", categories[[i]]))

                for (v in values) {
                    newc <- c(newc,
                        paste0(
                            fnames[i],
                            "[",
                            paste(setdiff(values, v), collapse = ","),
                            "]"
                        )
                    )
                }
            }
        }
    }
    

    oldc <- c(oldc, paste0("~", snames), snames)
    newc <- c(newc, paste0(snames, "[0]"), paste0(snames, "[1]"))
    expression <- replaceText(expression, oldc, newc)

    
    if (!is.null(noflevels)) {
        if (any(infodata$hastime)) {
            noflevels[infodata$hastime] <- noflevels[infodata$hastime] - 1
        }
        
        rnames <- colnames(validateNames(expression, snames = snames, data = data))
        noflevels <- noflevels[match(rnames, colnames(data))]
        
        if (any(noflevels > 2)) {
            stopError("Part(s) of the expression refer to multi-value data.")
        }
    }
    
    if (!is.null(dots$translate)) {
        return(
            list(
                expression = expression,
                oldc = oldc,
                newc = newc
            )
        )
    }

    return(expression)
    
}
