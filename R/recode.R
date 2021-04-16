`recode` <-
function(x, rules, cut, values, ...) {
    
    # TO DO: detect usage of both ; and , as rules separator, and generate error
    # TO DO: when input is a factor, cut values should be within the <order> of the ordered levels
    # and the output values should recode the <levels> in their exact <order>
    # ex. aa <- factor(rep(LETTERS[1:5], 20, replace = TRUE), levels = c("E", "C", "B", "D", "A"))
    # and recode(aa, cut = "B") should recode:
    # "E", "C" and "B" to 1
    # "D" and "A" to 2
    
    if (missing(x)) {
        cat("\n")
        stop(simpleError("Argument \"x\" is missing.\n\n"))
    }
    
    if (!is.atomic(x))   {
        cat("\n")
        stop(simpleError("The input \"x\" should be an atomic vector / factor.\n\n"))
    }
    
    if (all(is.na(x))) {
        cat("\n")
        stop(simpleError("All values are missing in x.\n\n"))
    }

    `getUniques` <- function(x) {
        if (is.factor(x)) {
            return(levels(x))
        }
        else {
            tagged <- tagged_na_value(x)
            return(sort_labelled(unique_labelled(x[!is.na(x) | tagged])))
        }
    }

    
    
    `getFromRange` <- function(a, b, uniques, xisnumeric) {
        alo <- identical(a, "lo")
        ahi <- identical(a, "hi")
        blo <- identical(b, "lo")
        bhi <- identical(b, "hi")

        atagged <- tagged_string(a)
        btagged <- tagged_string(b)

        if (sum(atagged, btagged) == 1) {
            cat("\n")
            stop(simpleError("Both numbers in a range have to be either tagged or not.\n\n"))
        }

        a <- ifelse(alo, uniques[1], a)
        b <- ifelse(blo, uniques[1], b)

        if (xisnumeric) {
            a <- ifelse(ahi, uniques[which.max(uniques)], a)
            b <- ifelse(bhi, uniques[which.max(uniques)], b)
            
            if (all(is.element(c(tag_from_string(a), tag_from_string(b)), letters))) {
                posa <- which(is.element(letters, tag_from_string(a)))
                posb <- which(is.element(letters, tag_from_string(b)))

                if (posa > posb) {
                    temp <- a
                    a <- b
                    b <- temp
                }
            }

            if (possibleNumeric(a) & possibleNumeric(b)) {
                # I could use this to check tagged NA strings like ".a"
                # as possibly numeric but it is not necessary
                a <- asNumeric(a)
                b <- asNumeric(b)
                if (a > b) {
                    temp <- a
                    a <- b
                    b <- temp
                }
            }
            
        }
        else {
            a <- ifelse(ahi, uniques[length(uniques)], a)
            b <- ifelse(bhi, uniques[length(uniques)], b)

            posa <- which(isElement(uniques, a))
            posb <- which(isElement(uniques, b))

            if (posa > posb) {
                temp <- a
                a <- b
                b <- temp
            }      
        }
        
        utagged <- tagged_na_value(uniques)
        if (atagged & any(utagged)) {
            a <- make_tagged_na(tag_from_string(a))
        }

        if (btagged & any(utagged)) {
            b <- make_tagged_na(tag_from_string(b))
        }

        seqfrom <- which(isElement(uniques, a))
        seqto <- which(isElement(uniques, b))
        
        
        if (any(c(utagged, atagged, btagged))) {
            temp2 <- unique_labelled(c(uniques, a, b), sort = TRUE)
        }
        else {
            temp2 <- sort(unique(c(uniques, a, b)))
        }
        
        if (length(seqfrom) == 0) {
            seqfrom <- which(isElement(uniques, temp2[which(isElement(temp2, a)) + 1]))
        }
        
        if (length(seqto) == 0) {
            seqto <- which(isElement(uniques, temp2[which(isElement(temp2, b)) - 1]))
        }
        
        if (length(c(seqfrom, seqto)) < 2) return(NULL)
        
        return(seq(seqfrom, seqto))
    }
    
    dots <- recreate(list(...))
    as.factor.result  <- if (is.element("as.factor.result",  names(dots))) dots$as.factor.result  else FALSE
    as.numeric.result <- if (is.element("as.numeric.result", names(dots))) dots$as.numeric.result else TRUE
    factor.levels     <- if (is.element("levels",            names(dots))) splitstr(dots$levels)  else c()
    factor.labels     <- if (is.element("labels",            names(dots))) splitstr(dots$labels)  else c()
    factor.ordered    <- FALSE

    if (is.element("ordered", names(dots))) {
        factor.ordered <- dots$ordered
    }
    else if (is.element("ordered_result", names(dots))) {
        factor.ordered <- dots$ordered_result
    }
    
    if (is.element("cuts", names(dots)) & missing(cut)) {
        cut <- dots[["cuts"]]
    }
    
    if (is.logical(factor.labels)) {
        factor.labels <- c()
    }

    if (!identical(factor.levels, c()) || !identical(factor.labels, c())) {
        as.factor.result <- TRUE
    }

    tagged <- tagged_na_value(x)
    
    if (missing(cut)) {
        
        rules <- gsub("\n|\t", "", gsub("'", "", gsub(")", "", gsub("c(", "", rules, fixed = TRUE))))
        if (length(rules) == 1) {
             rules <- unlist(strsplit(rules, split=";"))
        }
        
        rulsplit <- strsplit(rules, split="=")
        oldval <- unlist(lapply(lapply(rulsplit, trimstr), "[", 1))
        newval <- unlist(lapply(lapply(rulsplit, trimstr), "[", 2))
        
        temp <- rep(NA, length(x))
        
        elsecopy <- oldval == "else" & newval == "copy"

        if (any(elsecopy)) {
            if (is.factor(x)) {
                temp <- as.character(x)
            }
            else {
                temp <- x
                
                if (any(tagged)) {
                    temp <- as.character(x)
                    temp[tagged] <- get_na_tag(x[tagged])
                }
            }
            
            newval <- newval[!elsecopy]
            oldval <- oldval[!elsecopy]
        }

        newval[newval == "missing" | newval == "NA"] <- NA
        
        if (any(oldval == "else")) {
            if (sum(oldval == "else") > 1) {
                cat("\n")
                stop(simpleError("Too many \"else\" statements.\n\n"))
            }
            
            # place the "else" statement as the last one, very important
            whichelse <- which(oldval == "else")
            oldval <- c(oldval[-whichelse], oldval[whichelse])
            newval <- c(newval[-whichelse], newval[whichelse])
        }
        
        oldval <- lapply(lapply(lapply(oldval, strsplit, split=","), "[[", 1), function(y) {
            lapply(strsplit(y, split=":"), trimstr)
        })
        
        newval <- trimstr(rep(newval, unlist(lapply(oldval, length))))
        
        
        if (any(unlist(lapply(oldval, function(y) lapply(y, length))) > 2)) {
            cat("\n")
            stop(simpleError("Too many : sequence operators.\n\n"))
        }
        
        
        from <- unlist(lapply(oldval, function(y) lapply(y, "[", 1)))
        to <- unlist(lapply(oldval, function(y) lapply(y, "[", 2)))
        
        
        uniques <- getUniques(x)
        
        
        recoded <- NULL
        xisnumeric <- possibleNumeric(uniques)
        
        if (xisnumeric) {
            x <- asNumeric(x) # to be safe
            uniques <- getUniques(x)
        }
        

        for (i in seq(length(from))) {
            if (!is.na(to[i])) { # a range
                
                torecode <- getFromRange(from[i], to[i], uniques, xisnumeric)
                
                if (!is.null(torecode)) {
                    vals <- uniques[torecode]
                    temp[isElement(x, vals)] <- newval[i]
                    recoded <- c(recoded, vals)
                    tagged[isElement(x, vals)] <- tagged_string(newval[i])
                }
                
            }
            else { # a single value
                if (is.element(from[i], c("missing", "NA"))) {
                    tagged[is.na(x)] <- tagged_string(newval[i])
                }
                else {
                    tagged[isElement(x, from[i])] <- tagged_string(newval[i])
                }

                # "else" should (must?) be the last rule
                if (from[i] == "else") {
                    temp[!isElement(x, recoded)] <- newval[i]
                }
                else if (from[i] == "missing") {
                    temp[is.na(x)] <- newval[i]
                }
                else {
                    
                    # if (!any(x == from[i])) {
                    #     cat("\n")
                    #     val <- ifelse(is.na(suppressWarnings(as.numeric(from[i]))), paste("\"", from[i], "\"", sep = ""), from[i])
                    #     stop(simpleError(paste("The value", val, "was not found.\n\n", sep="")))
                    # }
                    
                    
                    if (tagged_string(newval[i])) {
                        newval[i] <- tag_from_string(newval[i])
                    }

                    temp[isElement(x, from[i])] <- newval[i]
                }
                recoded <- c(recoded, from[i])
            }
        }
        
    }
    else {
        
        if (length(cut) == 1 & is.character(cut)) {
            cutvalues <- gsub("\n|\t", "", gsub("'", "", gsub(")", "", gsub("c(", "", cut, fixed = TRUE))))
            cutvalues <- trimstr(unlist(strsplit(cutvalues, split = ",")))
            if (length(cutvalues) == 1) {
                cutvalues <- trimstr(unlist(strsplit(cutvalues, split = ";")))
            }
        }
        
        if (possibleNumeric(cutvalues)) {
            cutvalues <- asNumeric(cutvalues)
        }
        
        if (any(duplicated(cutvalues))) {
            cat("\n")
            stop(simpleError("Cut values should be unique.\n\n"))
        }
        
        if (missing(values)) {
            values <- seq(length(cutvalues) + 1)
        }
        else {
            if (length(values) == 1 & is.character(values)) {
                values <- gsub("\n|\t", "", gsub("'", "", gsub(")", "", gsub("c(", "", values, fixed = TRUE))))
                values <- trimstr(unlist(strsplit(values, split = ",")))
                if (length(values) == 1) {
                    values <- trimstr(unlist(strsplit(values, split = ";")))
                }
            }
            
            if (length(values) == length(cutvalues) + 1) {
                as.numeric.result <- possibleNumeric(values)
            }
            else {
                cat("\n")
                stop(simpleError(paste("There should be", length(cutvalues) + 1, "values for", length(cutvalues), ifelse(length(cutvalues) == 1, "cut.", "cutvalues."), "\n\n")))
            }
        }
        
        if (is.factor(x)) {
            lx <- levels(x)
            minx <- lx[1]
            maxx <- lx[length(lx)]
            
            if (is.numeric(cutvalues)) {
                insidex <- FALSE
            }
            else {
                insidex <- all(is.element(cutvalues, lx))
            }
        }
        else {
            sx <- sort(x)
            minx <- sx[1]
            maxx <- sx[length(x)]
            
            if (is.character(x) & is.numeric(cutvalues)) {
                insidex <- FALSE
            }
            else {
                insidex <- logical(length(cutvalues))
                for (i in seq(length(cutvalues))) {
                    insidex[i] <- cutvalues[i] >= minx & cutvalues[i] <= maxx
                }
            }
        }
        
        if (!all(insidex)) {
            cat("\n")
            stop(simpleError("Cut value(s) outside the input vector.\n\n"))
        }
        
        if (is.factor(x)) {
            nx <- as.numeric(x)
            nlx <- seq(length(lx))
            nc <- match(cutvalues, lx)
            temp <- rep(values[1], length(x))
            for (i in seq(length(cutvalues))) {
                temp[nx > nc[i]] = values[i + 1]
            }
        }
        else {
            temp <- rep(values[1], length(x))
            for (i in seq(length(cutvalues))) {
                temp[x > cutvalues[i]] = values[i + 1]
            }
        }

        if (identical(factor.labels, c()) & is.numeric(cutvalues)) {
            factor.labels <- values
        }
    }
    
    if (as.factor.result) {
        if (identical(factor.levels, c())) {
            factor.levels <- sort(unique(na.omit(temp)))
        }

        if (identical(factor.labels, c())) {
            factor.labels <- factor.levels
        }
        
        temp <- factor(temp, levels = factor.levels, labels = factor.labels, ordered = factor.ordered)
    }
    else if (as.numeric.result) {
        
        if (any(tagged)) {
            tags <- temp[tagged]
            temp[tagged] <- NA
        }

        if (possibleNumeric(temp)) {
            temp <- asNumeric(temp)
            if (any(tagged)) {
                temp[tagged] <- make_tagged_na(tag_from_string(tags))
                class(temp) <- c("haven_labelled", "vctrs_vctr", "double")
            }
        }
    }
    
    return(temp)
}
