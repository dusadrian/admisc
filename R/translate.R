`translate` <-
function(expression = "", snames = "", noflevels = NULL, data = NULL, ...) {
    
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    dots <- list(...)

    enter <- ifelse (is.element("enter", names(dots)), "",  "\n") # internal
    
    if (identical(expression, "")) {
        cat(enter)
        stop(simpleError(paste0("Empty expression.", enter, enter)))
    }
    
    if (any(grepl("<=>|<->|=>|->|<=|<-", expression))) {
        cat(enter)
        stop(simpleError(paste0("Incorrect expression, contains outcome and relation.", enter, enter)))
    }
    
    if (!is.vector(snames)) {
        cat(enter)
        stop(simpleError(paste0("Set names should be a single string or a vector of names.", enter, enter)))
    }

    if (!is.null(data)) {
        if (is.null(colnames(data))) {
            cat(enter)
            stop(simpleError(paste0("Data should have column names.", enter, enter)))
        }
    }

    if (is.null(data) & (identical(snames, "") | is.null(noflevels))) {
        # sc <- sys.calls()
        # syscalls <- unlist(lapply(lapply(sc, as.character), "[[", 1))
        # wth <- which(is.element(syscalls, "with"))
        
        # if (length(wth) == 0) {
        #     dc <- which(is.element(syscalls, "do.call"))
        #     if (length(dc) > 0) {
        #         checkx <- which(unlist(lapply(sc[dc], function(x) any(x == "with"))))
        #         if (length(checkx) > 0) {
        #             wth <- checkx[length(checkx)]
        #         }
        #     }
        # }
        
        # if (length(wth) > 0) {
        #     data <- get(unlist(lapply(lapply(sc[wth], as.character), "[[", 2)), envir = length(syscalls) - wth)
        # }
        
        syscalls <- as.character(sys.calls())
        if (length(withdata <- grep("with\\(", syscalls)) > 0) {
            withdata <- withdata[length(withdata)]
            data <- get(unlist(strsplit(gsub("with\\(", "", syscalls[withdata]), split = ","))[1], envir = length(syscalls) - withdata)
        }
    }
    
    if (!is.element("data.frame", class(data))) {
        data <- NULL
    }
    
    if (identical(snames, "")) {
        if (!is.null(data)) {
            snames <- colnames(data)
        }
    }
    else {

        # TO DO: see how this affects package venn, since toupper is now longer used
        snames <- splitstr(snames)
        
        if (!is.null(data)) {
            if (length(setdiff(snames, colnames(data))) > 0) {
                cat(enter)
                stop(simpleError(paste0("Part(s) of the \"snames\" not in the column names from the data.", enter, enter)))
            }
        }
    }
    
    if (is.null(noflevels)) {
        
        if (!is.null(data)) {
            # getInfo() not getLevels() because some data might have time information
            if (identical(snames, "")) {
                noflevels <- getInfo(data)$noflevels
            }
            else {
                noflevels <- getInfo(data[, snames, drop = FALSE])$noflevels
            }
        }
    }
    else {
        if (is.character(noflevels)) {
            noflevels <- splitstr(noflevels)
        }
    }

    replaced <- FALSE
    
    if (!identical(snames, "") & length(snames) > 0) {
        if (any(nchar(snames) > 1) & !is.element("validate", names(dots))) {
            
            snameso <- snames
            if (length(snames) < 27) {
                snamesr <- LETTERS[seq(length(snames))]
            }
            else {
                snamesr <- paste("X", seq(length(snames)), sep = "")
            }

            for (i in seq(length(expression))) {
                expression[i] <- replaceText(expression[i], snames, snamesr)
            }
            
            if (!is.null(data)) {
                positions <- c()
                # this code makes sure that column names
                # don't get overwritten multiple times
                # for instance if they are already single letters
                for (i in seq(length(snames))) {
                    colpos <- setdiff(which(colnames(data) == snames[i]), positions)
                    if (length(colpos) > 0) {
                        colnames(data)[colpos] <- snamesr[i]
                        positions <- c(positions, colpos)
                    }
                }
            }

            snames <- snamesr
            replaced <- TRUE
        }
    }
    
    # remove white space and any non-printable characters
    expression <- gsub("[[:space:]]|[^ -~]+", "", expression)
    
    
    if (identical("1-", substring(expression, 1, 2))) {
        explist <- list(input = gsub("1-", "", expression), snames = snames)
        if (!is.null(noflevels)) explist$noflevels <- noflevels
        expression <- do.call(negate, explist)
    }
    
    # ",[0-9]" to eliminate _only_ commas within curly brackets, like T{1,2}"
    if (any(grepl(",", gsub(",[0-9]", "", expression)))) {
        expression <- splitstr(expression)
    }
    
    arglist <- list(snames = snames)

    if (!is.null(noflevels)) {
        arglist$noflevels <- noflevels
    }
    
    expression <- unlist(lapply(expression, function(x) {
        if (grepl("[(|)]", x)) {
            x <- do.call(expandBrackets, c(list(expression = x), arglist)) # , data = data)
        }
        return(x)
    }))
    
    
    pporig <- trimstr(unlist(strsplit(expression, split="[+]")))
    multivalue <- any(grepl("\\[|\\]|\\{|\\}", expression))
    expression <- gsub("[[:space:]]", "", expression)
    
    beforemessage <- "Condition"
    aftermessage <- "does not match the set names from \"snames\" argument"
    
    
    if (is.element("validate", names(dots))) {
        if (is.null(data)) {
            beforemessage <- "Object"
            aftermessage <- "not found"
        }
        else {
            aftermessage <- "not found in the data"
        }
    }
    
    if (multivalue) {
        curly <- any(grepl("[{]", expression))
        expression <- gsub("[*]", "", expression)
        # return(list(expression = expression, snames = snames, noflevels = noflevels, data = data))
        checkMV(expression, snames = snames, noflevels = noflevels, data = data)
        
        # parse plus
        pp <- unlist(strsplit(expression, split = "[+]"))
        
        if (curly) {
            conds <- sort(unique(notilde(curlyBrackets(pp, outside=TRUE))))
        }
        else {
            conds <- sort(unique(notilde(squareBrackets(pp, outside=TRUE))))
        }

        if (identical(snames, "")) {
            if (!is.null(data)) {
                conds <- intersect(colnames(data), conds)
            }
        }
        else {
            if (all(is.element(conds, snames))) {
                conds <- snames
            }
            else {

                conds <- setdiff(conds, snames)

                if (length(conds) > 1) {
                    beforemessage <- paste(beforemessage, "s", sep = "")
                    aftermessage <- gsub("does", "do", aftermessage)
                }
                
                cat(enter)
                stop(simpleError(sprintf("%s '%s' %s.\n\n", beforemessage, paste(conds, collapse = ","), aftermessage)))
                
            }
        }
        
        if (any(hastilde(expression))) {
            if (is.null(noflevels)) {
                # for sure there is data, as checked by checkMV()
                # getInfo() not getLevels() because some data might have time information
                noflevels <- getInfo(data[, conds, drop = FALSE])$noflevels
            }
        }
        
        retlist <- lapply(pp, function(x) {
            
            if (curly) {
                outx <- curlyBrackets(x, outside = TRUE)
                inx <- lapply(curlyBrackets(x), splitstr)
            }
            else {
                outx <- squareBrackets(x, outside = TRUE)
                inx <- lapply(squareBrackets(x), splitstr)
            }

            remtilde <- notilde(outx)
            dupnot <- duplicated(remtilde)
            
            if (length(win <- which(hastilde(outx))) > 0) {
                for (i in win) {
                    inx[[i]] <- setdiff(seq(noflevels[which(is.element(conds, remtilde[i]))]) - 1, inx[[i]])
                }
            }
            
            empty <- FALSE
            
            for (i in seq(length(conds))) {
                if (is.element(conds[i], remtilde[dupnot])) { # that particular condition is duplicated
                    
                    wdup <- which(remtilde == conds[i])
                    inx[[wdup[1]]] <- intersect(inx[[wdup[1]]], inx[[wdup[2]]])
                    if (length(wdup) > 2) {
                        for (i in seq(3, length(wdup))) {
                            dupres <- intersect(dupres, inx[[wdup[i]]])
                        }
                    }
                    
                    if (length(inx[[wdup[1]]]) == 0) {
                        empty <- TRUE
                    #     cat(enter)
                    #     stop(simpleError(paste0("Non-intersecting levels in the same product.", enter, enter)))
                    }
                }
            }
            
            ret <- as.list(rep(-1, length(conds)))
            names(ret) <- conds
            
            ret[notilde(outx[!dupnot])] <- inx[!dupnot]
            
            return(ret)
        })
        
        names(retlist) <- pporig

        # this solves the bug related with the hypothetical and illogical:
        # simplify("A{1}*B{0}*B{1}", snames = LETTERS[1:2], noflevels = c(2,2))
        retlist <- retlist[!unlist(lapply(retlist, function(x) any(unlist(lapply(x, length)) == 0)))]

        if (length(retlist) == 0) {
            cat(enter)
            stop(simpleError(paste0("The result is an empty set.", enter, enter)))
        }
    }
    else {

        sl <- ifelse((replaced & length(snames) < 27) | identical(snames, ""), TRUE, all(nchar(snames) == 1))
        # parse plus
        pp <- unlist(strsplit(expression, split = "[+]"))
        
        if (replaced) {
            pp <- gsub("[*]", "", pp)
        }

        splitchar <- ifelse(any(grepl("[*]", pp)) | !sl, "[*]", "")
        conds <- setdiff(sort(unique(notilde(unlist(strsplit(pp, split = splitchar))))), "")
        
        if (!identical(snames, "")) {
            
            if (!is.null(data)) {
                
                if (all(is.element(conds, snames)) & all(is.element(conds, colnames(data)))) {
                    
                    infodata <- getInfo(data[, conds, drop = FALSE])
                    
                    valid <- which(infodata$noflevels >= 2)
                    invalid <- !any(infodata$hastime[valid]) & any(infodata$noflevels[valid] > 2)
                    
                    if (invalid) {
                        cat(enter)
                        stop(simpleError(paste0("Expression should be multi-value, since it refers to multi-value data.", enter, enter)))
                    }
                }
            }
            
            
            if (all(is.element(conds, snames))) {
                conds <- snames
            }
            else {
                conds <- setdiff(conds, snames)
                if (length(conds) > 1) {
                    beforemessage <- paste(beforemessage, "s", sep = "")
                    aftermessage <- gsub("does", "do", aftermessage)
                }
                
                if (replaced) {
                    conds <- replaceText(conds, snames, snameso)
                }

                cat(enter)
                stop(simpleError(sprintf("%s '%s' %s.\n\n", beforemessage, paste(conds, collapse = ","), aftermessage)))
            }
            
        }
        
        retlist <- lapply(pp, function(x) {
            
            x <- unlist(strsplit(x, split = splitchar))
            
            # for those situations when conditions are single letters (thus splitchar is "")
            # but are negated with a tilde, e.g. "~AB" shoud be "~A" and "B" but it is "~", "A", "B"
            if (length(wx <- which(x == "~")) > 0) {
                x[wx + 1] <- paste0("~", x[wx + 1])
                x <- x[-wx]
            }

            x <- unique(x)
            remtilde <- notilde(x)
            dup <- remtilde[duplicated(remtilde)]
            x <- x[!is.element(remtilde, dup)]

            ret <- as.list(rep(-1, length(conds)))
            names(ret) <- conds

            ret[notilde(x)] <- 1 - hastilde(x)
            
            return(ret)
        })
        
        names(retlist) <- pporig
        
    } # non-multivalue
    
    
    retlist <- retlist[!unlist(lapply(retlist, function(x) all(unlist(x) < 0)))]
    
    if (replaced) {
        
        for (i in seq(length(retlist))) {
            # snames is now snamesr
            names(retlist)[i] <- replaceText(names(retlist)[i], snames, snameso)
            names(retlist[[i]]) <- snameso
        }
    }

    # checkl <- createMatrix(rep(2, length(retlist)))
    # checkl <- apply(checkl[rowSums(checkl) == 2, ], 1, function(x) which(x == 1))
    # checkl <-
    
    retmat <- do.call(rbind, lapply(retlist, function(x) {
        xnames <- names(x)
        x <- unlist(lapply(x, paste, collapse = ","))
        names(x) <- xnames
        return(x)
    }))
    
    if (length(retmat) == 0) {
        cat(enter)
        stop(simpleError(paste0("Impossible to translate an empty set.", enter, enter)))
    }
    
    if (is.element("retlist", names(dots))) {
        attr(retmat, "retlist") <- retlist
    }
    
    class(retmat) <- c("matrix", "admisc_translate")
    return(retmat)
}
