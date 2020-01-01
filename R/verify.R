`verify` <-
function(data) {
    if (is.data.frame(data)) {
        if (is.null(colnames(data))) {
            cat("\n")
            stop(simpleError("The dataset doesn't have any columns names.\n\n"))
        }
        
        # determine if it's a valid QCA dataframe
        checkNumUncal <- lapply(data, function(x) {
            # making sure it's not a temporal QCA column
            x <- setdiff(x, c("-", "dc", "?"))
            pn <- possibleNumeric(x)
            
            uncal <- mvuncal <- FALSE
            
            if (pn) {
                y <- na.omit(asNumeric(x))
                if (any(y > 1) & any(abs(y - round(y)) >= .Machine$double.eps^0.5)) {
                    uncal <- TRUE
                }
                
                if (length(seq(0, max(y))) > 20) {
                    mvuncal <- TRUE
                }
            }
            
            return(c(pn, uncal, mvuncal))
        })
        
        checknumeric <- sapply(checkNumUncal, "[[", 1)
        checkuncal <- sapply(checkNumUncal, "[[", 2)
        checkmvuncal <- sapply(checkNumUncal, "[[", 3)
        
        if (!all(checknumeric)) {
            cat("\n")
            notnumeric <- colnames(data)[!checknumeric]
            errmessage <- paste("The causal condition",
                                ifelse(length(notnumeric) == 1, " ", "s "),
                                paste(notnumeric, collapse=", "),
                                ifelse(length(notnumeric) == 1, " is ", " are "),
                                "not numeric.", sep="")
            stop(simpleError(paste(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep=""), "\n\n", sep = "")))
        }
        
        if (any(checkuncal)) {
            cat("\n")
            uncalibrated <- colnames(data)[checkuncal]
            errmessage <- paste("Uncalibrated data.\n",
            "Fuzzy sets should have values bound to the interval [0 , 1] and all other sets should be crisp.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep="")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
        }
        
        if (any(checkmvuncal)) {
            cat("\n")
            uncalibrated <- colnames(data)[checkmvuncal]
            errmessage <- paste("Possibly uncalibrated data.\n",
            "Multivalue conditions with more than 20 levels are unlikely to be (properly) calibrated.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep="")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
        }
                   
    }
    else if (is.vector(data)) {
        if (!possibleNumeric(data)) {
            cat("\n")
            stop(simpleError("Non numeric input.\n\n"))
        }
    }
}
