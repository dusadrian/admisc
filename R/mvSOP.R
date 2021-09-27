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

    if (is.null(data)) {
        checkValid(expression = expression, snames = snames)
    }
    else {
        checkValid(expression = expression, snames = snames, data = data)

        infodata <- getInfo(data)

        if (any(infodata$factor)) {
            factors <- infodata$factors
            snames <- setdiff(snames, names(factors))
            oldc <- newc <- c()

            for (i in seq(length(factors))) {
                values <- factors[[i]]
                oldc <- c(oldc, names(factors[[i]]))
                newc <- c(newc, paste0(names(factors)[i], "[", values, "]"))
                
                
                if (!keep.tilde) {
                    oldc <- c(oldc, paste0("~", names(factors[[i]])))

                    for (v in values) {
                        newc <- c(newc,
                            paste0(
                                names(factors)[i],
                                "[",
                                paste(setdiff(values, v), collapse = ","),
                                "]"
                            )
                        )
                    }
                }
            }
        }
    }

    if (!identical(snames, "") & length(snames) > 0) {
        if (!is.null(noflevels)) {
            noflevels <- noflevels[match(snames, colnames(data))]
            if (any(noflevels) > 2) {
                stopError("Part(s) of the expression refer to multi-value data.")
            }
        }
        oldc <- c(oldc, paste0("~", snames), snames)
        newc <- c(newc, paste0(snames, "[0]"), paste0(snames, "[1]"))
    }

    expression <- replaceText(expression, oldc, newc)
    
    if (is.element("translate", names(dots))) {
        return(list(expression = expression, oldc = newc, newc = oldc))
    }

    return(expression)
    
}


`as.cs` <- function (
    expression = "", snames = "", data = NULL
) {
    if (!any(grepl("\\[|\\]|\\{|\\}", expression))) {
        stopError("The expression does not contain multi-value notation.")
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
    
    if (is.null(data)) {
        checkValid(expression = expression, snames = snames)
    }
    else {
        checkValid(expression = expression, snames = snames, data = data)

        infodata <- getInfo(data)

    }

}
