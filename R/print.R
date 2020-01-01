
`print.deMorgan` <- function(x, ...) {
    
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    pM <- paste("M", prettyNums, sep = "")
    
    if (!is.null(isol <- attr(x, "isol"))) {
        pM <- paste(pM, isol, sep = "-")
    }
    
    pM <- paste(pM, ": ", sep = "")
    
    cat("\n")

    if (length(x) == 1 & !attr(x, "minimized")) {
        fx <- x[[1]]

        if (is.null(fx)) {
            cat("No negation possible.\n")
        }
        else {
            for (j in seq(length(fx))) {
                prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                cat(paste("N", prettyNumsFact[j], ": ", sep = ""))
                flength <- nchar(prettyNumsFact[j]) + 1
                strvctr <- unlist(strsplit(fx[j], split = " + "))
                cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
            }
            cat("\n")
        }

    }
    else {
        for (i in seq(length(x))) {
            cat(paste(pM[i], names(x)[i], sep = ""), "\n")
            fx <- x[[i]]
            
            if (is.null(fx)) {
                cat("No negation possible.\n")
            }
            else {
                for (j in seq(length(fx))) {
                    prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                    cat(paste("  N", prettyNumsFact[j], ": ", sep = ""))
                    flength <- nchar(prettyNumsFact[j]) + 3
                    strvctr <- unlist(strsplit(fx[j], split = " + "))
                    cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
                }
                cat("\n")
            }
        }
    }
}

`print.intersection` <- function(x, ...) {
    
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    
    pI <- paste("E", prettyNums, sep="")
    pO <- paste("  I", prettyNums, sep="")
    
    if (!is.null(isol <- attr(x, "isol"))) {
        pI <- paste(pI, isol, sep = "-")
        pO <- paste(pO, isol, sep = "-")
    }
    
    pI <- paste(pI, ": ", sep = "")
    pO <- paste(pO, ": ", sep = "")
    
    
    
    expressions <- attr(x, "expressions")
    ncharSI <- max(nchar(pI))
    
    
    for (i in seq(length(x))) {
        
        cat("\n", pI[i], sep = "")
        cat(admisc::prettyString(expressions[i], getOption("width") - ncharSI, ncharSI, "+"))
        cat("\n", pO[i], sep = "")
        cat(admisc::prettyString(x[i], getOption("width") - ncharSI, ncharSI, "+"))
        cat("\n")
    }
    
    cat("\n")
}

`print.simplify` <- function(x, ...) {
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    
    cat("\n")

    if (all(x == "")) {
        cat("S1: \"\"\n")
    }
    else {
        for (i in seq(length(x))) {
            cat(paste("S", prettyNums[i], ": ", sep = ""))
            flength <- nchar(prettyNums[i]) + 1
            strvctr <- unlist(strsplit(x[i], split = " + "))
            cat(prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n")
        }
    }

    cat("\n")
}

`print.factorize` <- function(x, ...) {
    
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    pM <- paste("M", prettyNums, sep = "")
    
    if (!is.null(isol <- attr(x, "isol"))) {
        pM <- paste(pM, isol, sep = "-")
    }
    
    pM <- paste(pM, ": ", sep = "")
    
    cat("\n")

    if (length(x) == 1) {
        fx <- x[[1]]

        if (is.null(fx)) {
            cat("No factorization possible.\n")
        }
        else {
            for (j in seq(length(fx))) {
                prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                cat(paste("F", prettyNumsFact[j], ": ", sep = ""))
                flength <- nchar(prettyNumsFact[j]) + 1
                strvctr <- unlist(strsplit(fx[j], split = " + "))
                cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
            }
            cat("\n")
        }

    }
    else {
        for (i in seq(length(x))) {
            cat(paste(pM[i], names(x)[i], sep = ""), "\n")
            fx <- x[[i]]
            
            if (is.null(fx)) {
                cat("No factorization possible.\n")
            }
            else {
                for (j in seq(length(fx))) {
                    prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                    cat(paste("  F", prettyNumsFact[j], ": ", sep = ""))
                    flength <- nchar(prettyNumsFact[j]) + 3
                    strvctr <- unlist(strsplit(fx[j], split = " + "))
                    cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
                }
                cat("\n")
            }
        }
    }
}

`print.translate` <- function(x, ...) {
    other.args <- list(...)
    
    cat("\n")
    original <- FALSE
    y <- matrix(as.vector(x), nrow=nrow(x))
    if (is.element("original", names(other.args))) {
        if (is.logical(other.args$original)) {
            original <- other.args$original[1]
        }
    }
    
    cols <- colnames(x)
    colnames(y) <- cols
    
    if (original) {
        minus <- any(y < 0)
        if (minus) {
            y[y >= 0] <- paste("", y[y >= 0])
            cols[nchar(cols) == 1] <- paste("", cols[nchar(cols) == 1])
            colnames(y) <- cols
        }
    }
    else {
        y[x < 0] <- ""
    }
    
    rownames(y) <- paste(rownames(x), " ")
    print(prettyTable(y))
    cat("\n")
}

