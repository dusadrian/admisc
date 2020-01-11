`checkMV` <- function(expression, snames = "", noflevels = NULL, data = NULL) {
    
    # check to see if opened brackets have closing brackets
    if (length(unlist(gregexpr("[{]+", expression))) != length(unlist(gregexpr("[}]+", expression)))) {
        cat("\n")
        stop(simpleError("Incorrect expression, opened and closed brackets don't match.\n\n"))
    }
    
    # whatever it is outside the curly brackets must have the same length
    # as the information inside the curly brackets
    
    # remove everything except snames' names and the brackets
    tempexpr <- gsub("[*|,|;|(|)]", "", expression)
    pp <- trimstr(unlist(strsplit(tempexpr, split = "[+]")))
    
    insb <- curlyBrackets(gsub("[*|(|)]", "", expression))
    tempexpr <- curlyBrackets(tempexpr, outside = TRUE)
    
    
    if (length(insb) != length(tempexpr)) {
        cat("\n")
        stop(simpleError("Incorrect expression, some set names do not have brackets.\n\n"))
    }
    
    if (any(grepl("[a-zA-Z]", gsub("[,|;]", "", insb)))) {
        cat("\n")
        stop(simpleError("Invalid {multi}values, levels should be numeric.\n\n"))
    }
    
    conds <- sort(unique(notilde(curlyBrackets(pp, outside = TRUE))))
    
    if (is.null(data)) {
        if (is.null(noflevels)) {
            if (any(hastilde(expression))) {
                cat("\n")
                stop(simpleError("Negating a multivalue condition requires the number of levels.\n\n"))
            }
        }
        else {
            if (identical(snames, "")) {
                cat("\n")
                stop(simpleError("Cannot verify the number of levels without the set names.\n\n"))
            }
            
            snames <- splitstr(snames)
            if (is.character(noflevels)) {
                noflevels <- splitstr(noflevels)
            }
            
            if (length(snames) != length(noflevels)) {
                cat("\n")
                stop(simpleError("Length of the set names differs from the length of the number of levels.\n\n"))
            }
            for (i in seq(length(tempexpr))) {
                
                if (!is.element(notilde(tempexpr[i]), snames)) {
                    cat("\n")
                    stop(simpleError(sprintf("Condition %s not present in the set names.\n\n", tempexpr[i])))
                }
                if (max(asNumeric(splitstr(insb[i]))) > noflevels[match(notilde(tempexpr[i]), snames)] - 1) {
                    cat("\n")
                    stop(simpleError(sprintf("Levels outside the number of levels for condition %s.\n\n", tempexpr[i])))
                }
            }
        }
    }
    else { # the data is present
        # if (identical(snames, "")) {
            if (length(setdiff(conds, colnames(data))) > 0) {
                cat("\n")
                stop(simpleError("Parts of the expression don't match the column names from \"data\" argument.\n\n"))
            }
        # }
    }
    
    if (!identical(snames, "")) {
        if (length(setdiff(conds, splitstr(snames))) > 0) {
            cat("\n")
            stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
        }
    }
}
