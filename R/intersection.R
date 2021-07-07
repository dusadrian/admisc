`intersection` <- function(..., snames = "", noflevels = NULL) {
    
    # (function(...)substitute(...()))(x = A, y = B)
    # $x
    # A

    # $y
    # B

    # (function(...)substitute(list(...)))(x = A, y = B)
    # list(x = A, y = B)

    # is.list(aa)
    # [1] TRUE
    # names(aa)
    # [1] "x" "y"
    # is.list(bb)
    # [1] FALSE
    # bb[[2]] # si totusi se comporta ca o lista
    # A
    # names(bb)
    # [1] ""  "x" "y"
    
    dots <- substitute(list(...))
    
    if (length(dots) > 1) {
        # lapply(dots, recreate) messes up with parent.frame(), I think
        # because it introduces an yet another parent before recreate()
        for (i in seq(2, length(dots))) {
            dots[[i]] <- recreate(dots[[i]])
        }
    }
    
    dots <- eval(dots)
    
    snames <- recreate(substitute(snames))
    
    if (length(dots) == 0) {
        stopError("Nothing to intersect.")
    }

    ### probably unnecessary hack to allow package admisc being checked without package QCA
    # e.g. via negate()
    if (length(dots[[1]]) == 0) {
        return(invisible(character(0)))
    }
    ###
    
    snames <- splitstr(snames)
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))
    
    
    isol <- NULL
    
    for (i in seq(length(dots))) {
        x <- dots[[i]]

        if (methods::is(dots[[i]], "QCA_min")) {
            
            if (identical(snames, "")) {
                snames <- dots[[i]]$tt$options$conditions
                if (dots[[i]]$options$use.letters) {
                    snames <- LETTERS[seq(length(snames))]
                }
            }
            
            if (is.element("i.sol", names(x))) {
                elengths <- unlist(lapply(dots[[i]]$i.sol, function(x) length(x$solution)))
                isol <- paste(rep(names(dots[[i]]$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
                
                dots[[i]] <- as.vector(unlist(lapply(dots[[i]]$i.sol, function(x) {
                    lapply(x$solution, paste, collapse = " + ")
                })))
            }
            else {
                dots[[i]] <- as.vector(unlist(lapply(dots[[i]]$solution, paste, collapse = " + ")))
            }
        }
        else if (methods::is(dots[[i]], "admisc_deMorgan")) {

            isol <- attr(x, "isol")
            
            dots[[i]] <- unlist(x)

            if (!is.null(attr(x, "snames"))) {
                attr(dots[[i]], "snames") <- attr(x, "snames")
            }
        
            if (!is.null(attr(x, "isol"))) {
                attr(dots[[i]], "isol") <- attr(x, "isol")
            }

            attr(dots[[i]], "minimized") <- attr(x, "minimized")
        }
        
        if (!is.character(dots[[i]])) {
            stopError("Unrecognised input.")
        }
    }
    
    arglist <- list(snames = snames)
    
    if (!is.null(noflevels)) {
        arglist$noflevels <- noflevels
    }
    
    if (requireNamespace("QCA", quietly = TRUE)) {
        combs <- QCA::createMatrix(unlist(lapply(dots, length)))
    } else {
        combs <- getMatrix(unlist(lapply(dots, length)))
    }


    expressions <- result <- character(nrow(combs))
    
    conj <- ifelse(sl, "", "*")
    
    for (i in seq(nrow(combs))) {
        x <- combs[i, ] + 1
        expression <- c()
        for (j in seq(length(x))) {
            expression <- c(expression, dots[[j]][x[j]])
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
    
    class(result) <- c("character", "admisc_intersection")
    return(result)
}
