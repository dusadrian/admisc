`checkMV` <- function(
    expression, snames = "", noflevels = NULL, data = NULL, use.labels = FALSE, categories = list(), ...
) {
    
    curly <- any(grepl("[{]", expression))
    # check to see if opened brackets have closing brackets
    if (length(unlist(gregexpr(ifelse(curly, "[{]+", "\\[+"), expression))) != length(unlist(gregexpr(ifelse(curly, "[}]+", "\\]+"), expression)))) {
        stopError("Incorrect expression, opened and closed brackets don't match.")
    }

    dots <- list(...)

    if (is.element("categorical", names(dots))) {
        use.labels <- dots$categorical
        dots$categorical <- NULL
    }

    # whatever it is outside the curly brackets must have the same length
    # as the information inside the curly brackets
    
    # remove everything except snames' names and the round brackets
    tempexpr <- gsub("[*|,|;|(|)]", "", expression)
    pp <- trimstr(unlist(strsplit(tempexpr, split = "[+]")))

    if (curly) {
        insb <- curlyBrackets(gsub("[*|(|)]", "", expression))
        tempexpr <- curlyBrackets(tempexpr, outside = TRUE)
    }
    else {
        insb <- squareBrackets(gsub("[*|(|)]", "", expression))
        tempexpr <- squareBrackets(tempexpr, outside = TRUE)
    }
    
    if (length(insb) != length(tempexpr)) {
        error <- TRUE
        if (use.labels) {
            tempexpr2 <- tempexpr[!is.element(tempexpr, names(unlist(unname(categories))))]
            error <- length(insb) != length(tempexpr2)
        }
        
        if (error) {
            stopError("Incorrect expression, some set names do not have brackets.")
        }
    }
    
    if (any(grepl("[a-zA-Z]", gsub("[,|;]", "", insb)))) {
        stopError("Invalid [multi]values, levels should be numeric.")
    }
    
    if (curly) {
        conds <- sort(unique(notilde(curlyBrackets(pp, outside = TRUE))))
    }
    else {
        conds <- sort(unique(notilde(squareBrackets(pp, outside = TRUE))))
    }
    
    if (is.null(data)) {
        if (is.null(noflevels)) {
            if (any(hastilde(expression))) {
                stopError("Negating a multivalue condition requires the number of levels.")
            }
        }
        else {
            if (identical(snames, "")) {
                stopError("Cannot verify the number of levels without the set names.")
            }
            
            snames <- splitstr(snames)
            if (is.character(noflevels)) {
                noflevels <- splitstr(noflevels)
            }

            # temporary fix until version 3.14 of package QCA is published
            if (length(noflevels) == 1 && is.numeric(noflevels) && length(snames) > 1) {
                noflevels <- rep(noflevels, length(snames))
            }
            # end temporary fix
            
            if (length(snames) != length(noflevels)) {
                stopError("Length of the set names differs from the length of the number of levels.")
            }
            
            for (i in seq(length(tempexpr))) {
                
                if (!is.element(notilde(tempexpr[i]), snames)) {
                    # print(list(expression, tempexpr, snames))
                    stopError(sprintf("Condition %s not present in the set names.", tempexpr[i]))
                }
                if (max(asNumeric(splitstr(insb[i]))) > noflevels[match(notilde(tempexpr[i]), snames)] - 1) {
                    stopError(sprintf("Levels outside the number of levels for condition %s.", tempexpr[i]))
                }
            }
        }
    }
    
    for (i in seq(length(expression))) {
        checkValid(
            expression = expression[i],
            snames = "something", # doesn't matter here
            data = data,
            categories = categories
        )
    }
}
