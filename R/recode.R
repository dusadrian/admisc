`recode` <-
function(x, rules, cuts, values, ...) {
    
    # TO DO: detect usage of both ; and , as rules separator, and generate error
    # TO DO: when input is a factor, cuts should be within the <order> of the ordered levels
    # and the output values should recode the <levels> in their exact <order>
    # ex. aa <- factor(rep(LETTERS[1:5], 20, replace = TRUE), levels = c("E", "C", "B", "D", "A"))
    # and recode(aa, cuts = "B") should recode:
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
    
    dots <- recreate(list(...))
    as.factor.result  <- if (is.element("as.factor.result",  names(dots))) dots$as.factor.result  else FALSE
    as.numeric.result <- if (is.element("as.numeric.result", names(dots))) dots$as.numeric.result else TRUE
    factor.ordered    <- if (is.element("ordered",           names(dots))) dots$ordered           else FALSE
    factor.ordered    <- if (is.element("ordered_result",    names(dots))) dots$ordered_result    else FALSE
    factor.levels     <- if (is.element("levels",            names(dots))) splitstr(dots$levels)  else c()
    factor.labels     <- if (is.element("labels",            names(dots))) splitstr(dots$labels)  else c()
    
    if (is.logical(factor.labels)) {
        factor.labels <- c()
    }

    if (!identical(factor.levels, c()) || !identical(factor.labels, c())) {
        as.factor.result  <- TRUE
    }
    
    getFromRange <- function(a, b) {
        
        copya <- a
        copyb <- b
        
        a <- ifelse(a == "lo", uniques[1], a)
        b <- ifelse(b == "hi", uniques[length(uniques)], b)
        
        if (xisnumeric) {
            a <- asNumeric(a)
            b <- asNumeric(b)
            if (a > b & (copya == "lo" | copyb == "hi")) return(NULL)
        }
        
        seqfrom <- which(uniques == a)
        seqto <- which(uniques == b)
        
        temp2 <- sort(unique(c(uniques, a, b)))
        
        if (length(seqfrom) == 0) {
            seqfrom <- which(uniques == temp2[which(temp2 == a) + 1])
        }
        
        if (length(seqto) == 0) {
            seqto <- which(uniques == temp2[which(temp2 == b) - 1])
        }
        
        if (length(c(seqfrom, seqto)) < 2) return(NULL)
        
        return(seq(seqfrom, seqto))
    }
    
    if (missing(cuts)) {
        
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
        
        
        uniques <- if(is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
        
        recoded <- NULL
        xisnumeric <- possibleNumeric(uniques)
        
        if (xisnumeric) {
            x <- asNumeric(x) # to be safe
            uniques <- sort(unique(x[!is.na(x)]))
        }
        
        
        for (i in seq(length(from))) {
            if (!is.na(to[i])) { # a range
                torecode <- getFromRange(from[i], to[i])
                if (!is.null(torecode)) {
                    vals <- uniques[torecode]
                    temp[x %in% vals] <- newval[i]
                    recoded <- c(recoded, vals)
                }
                
            }
            else { # a single value
                
                # "else" should (must?) be the last rule
                if (from[i] == "else") {
                    temp[!is.element(x, recoded)] <- newval[i]
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
                    temp[x == from[i]] <- newval[i]
                }
                
                recoded <- c(recoded, from[i])
            }
        }
    }
    else {
        
        if (length(cuts) == 1 & is.character(cuts)) {
            cuts <- gsub("\n|\t", "", gsub("'", "", gsub(")", "", gsub("c(", "", cuts, fixed = TRUE))))
            cuts <- trimstr(unlist(strsplit(cuts, split = ",")))
            if (length(cuts) == 1) {
                cuts <- trimstr(unlist(strsplit(cuts, split = ";")))
            }
        }
        
        if (possibleNumeric(cuts)) {
            cuts <- asNumeric(cuts)
        }
        
        if (any(duplicated(cuts))) {
            cat("\n")
            stop(simpleError("Cut values should be unique.\n\n"))
        }
        
        if (missing(values)) {
            values <- seq(length(cuts) + 1)
        }
        else {
            if (length(values) == 1 & is.character(values)) {
                values <- gsub("\n|\t", "", gsub("'", "", gsub(")", "", gsub("c(", "", values, fixed = TRUE))))
                values <- trimstr(unlist(strsplit(values, split = ",")))
                if (length(values) == 1) {
                    values <- trimstr(unlist(strsplit(values, split = ";")))
                }
            }
            
            if (length(values) == length(cuts) + 1) {
                as.numeric.result <- possibleNumeric(values)
            }
            else {
                cat("\n")
                stop(simpleError(paste("There should be", length(cuts) + 1, "values for", length(cuts), ifelse(length(cuts) == 1, "cut.", "cuts."), "\n\n")))
            }
        }
        
        if (is.factor(x)) {
            lx <- levels(x)
            minx <- lx[1]
            maxx <- lx[length(lx)]
            
            if (is.numeric(cuts)) {
                insidex <- FALSE
            }
            else {
                insidex <- all(is.element(cuts, lx))
            }
        }
        else {
            sx <- sort(x)
            minx <- sx[1]
            maxx <- sx[length(x)]
            
            if (is.character(x) & is.numeric(cuts)) {
                insidex <- FALSE
            }
            else {
                insidex <- logical(length(cuts))
                for (i in seq(length(cuts))) {
                    insidex[i] <- cuts[i] >= minx & cuts[i] <= maxx
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
            nc <- match(cuts, lx)
            temp <- rep(values[1], length(x))
            for (i in seq(length(cuts))) {
                temp[nx > nc[i]] = values[i + 1]
            }
        }
        else {
            temp <- rep(values[1], length(x))
            for (i in seq(length(cuts))) {
                temp[x > cuts[i]] = values[i + 1]
            }
        }

        if (identical(factor.labels, c()) & is.numeric(cuts)) {
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
        if (possibleNumeric(temp)) {
            temp <- asNumeric(temp)
        }
    }
    
    return(temp)
}
