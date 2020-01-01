`intersection` <- function(..., snames = "", noflevels = NULL) {
    
    allargs <- list(...)
    
    if (length(allargs) == 0) {
        cat("\n")
        stop(simpleError("Nothing to intersect.\n\n"))
    }
    
    snames <- splitstr(snames)
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))
    
    
    isol <- NULL
    
    for (i in seq(length(allargs))) {
        x <- allargs[[i]]

        if (methods::is(allargs[[i]], "qca")) {
            
            if (identical(snames, "")) {
                snames <- allargs[[i]]$tt$options$conditions
                if (allargs[[i]]$options$use.letters) {
                    snames <- LETTERS[seq(length(snames))]
                }
            }
            
            if (is.element("i.sol", names(x))) {
                elengths <- unlist(lapply(allargs[[i]]$i.sol, function(x) length(x$solution)))
                isol <- paste(rep(names(allargs[[i]]$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
                
                allargs[[i]] <- as.vector(unlist(lapply(allargs[[i]]$i.sol, function(x) {
                    lapply(x$solution, paste, collapse = " + ")
                })))
            }
            else {
                allargs[[i]] <- as.vector(unlist(lapply(allargs[[i]]$solution, paste, collapse = " + ")))
            }
        }
        else if (methods::is(allargs[[i]], "deMorgan")) {

            isol <- attr(x, "isol")
            
            allargs[[i]] <- unlist(x)

            if (!is.null(attr(x, "snames"))) {
                attr(allargs[[i]], "snames") <- attr(x, "snames")
            }
        
            if (!is.null(attr(x, "isol"))) {
                attr(allargs[[i]], "isol") <- attr(x, "isol")
            }

            attr(allargs[[i]], "minimized") <- attr(x, "minimized")
        }
        
        if (!is.character(allargs[[i]])) {
            cat("\n")
            stop(simpleError("Unrecognised input.\n\n"))
        }
    }
    
    arglist <- list(snames = snames)
    
    if (!is.null(noflevels)) {
        arglist$noflevels <- noflevels
    }
    
    if (requireNamespace("QCA", quietly = TRUE)) {
        combs <- QCA::createMatrix(unlist(lapply(allargs, length)))
    } else {
        combs <- getMatrix(unlist(lapply(allargs, length)))
    }

    expressions <- result <- character(nrow(combs))
    
    conj <- ifelse(sl, "", "*")
    
    for (i in seq(nrow(combs))) {
        x <- combs[i, ] + 1
        expression <- c()
        for (j in seq(length(x))) {
            expression <- c(expression, allargs[[j]][x[j]])
        }
        
        disj <- grepl("[+]", expression)
        if (any(disj)) {
            expression[disj] <- paste("(", expression[disj], ")", sep = "")
        }
        
        if (any(!disj)) {
            ndisj <- which(!disj)
            if (any(ndisj == 1)) {
                expression[1] <- paste(expression[1], conj, sep = "")
            }
            if (any(ndisj == length(expression))) {
                expression[length(expression)] <- paste(conj, expression[length(expression)], sep = "")
            }
            
            if (length(ndisj <- setdiff(ndisj, c(1, length(expression)))) > 0) {
                expression[ndisj] <- paste(conj, expression[ndisj], conj, sep = "")
            }
        }
        
        expressions[i] <- paste(expression, collapse = "")
        # just to make sure this doesn't slip through
        expressions[i] <- gsub("\\*\\(", "(", expressions[i])
        
        result[i] <- do.call(expandBrackets, c(list(expressions[i]), arglist))
    }
    
    if (sl) {
        for (i in seq(length(expressions))) {
            result[i] <- gsub("[*]", "", result[i])
        }
    }
    
    attr(result, "expressions") <- expressions
    
    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }
    
    class(result) <- c("character", "intersection")
    return(result)
}

