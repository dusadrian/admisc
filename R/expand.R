`expand` <- function(expression = "", snames = "", noflevels = NULL,
    partial = FALSE, implicants = FALSE, ...) {
# `expand` <- function(x, noflevels = NULL) {

    # expression <- recreate(substitute(expression))
    # if (!identical(expression, "")) {
    #     expression <- tryCatch(eval(parse(text = expression), envir = parent.frame()),
    #         error = function(e) expression)
    # }

    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))

    dots <- list(...)
    enter <- ifelse(is.element("enter", names(dots)), "",  "\n") # internal
    
    multivalue <- FALSE
    
    scollapse <- ifelse(is.element("scollapse", names(dots)), dots$scollapse, FALSE) # internal collapse method
    scollapse <- scollapse | grepl("[*]", expression)

    if (!is.null(noflevels)) {
        if (is.character(noflevels) & length(noflevels) == 1) {
            noflevels <- splitstr(noflevels)
        }
    }
    # there is a dedicated function removeRedundants() in package QCA
    # written in C, just too much circular dependency
    `remred` <- function(x) {
        # function to remove redundant terms

        if (nrow(x) > 1) {
                
            redundant <- logical(nrow(x))
            
            for (i in seq(nrow(x) - 1)) {
                if (!redundant[i]) {
                    for (j in seq(i + 1, nrow(x))) {
                        if (!redundant[j]) {
                            subsetrow <- checkSubset(x[c(i, j), , drop = FALSE])
                            if (!is.null(subsetrow)) {
                                redundant[c(i, j)[subsetrow]] <- TRUE
                            }
                        }
                    }
                }
            }
            
            x <- x[!redundant, , drop = FALSE]
        }

        return(x)
    }
    
    `dnf` <- function(x, noflevels = NULL, partial = FALSE) {
        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(x))
        }

        # check if any column has all values equal to zero
        # which means there is no point to expand there

        zeroc <- which(apply(x, 2, function(x) all(x == 0)))
        if (length(zeroc) > 0 & partial) {
            x <- x[, -zeroc, drop = FALSE]
        }


        result <- matrix(nrow = 0, ncol = ncol(x))
        rmin <- min(apply(x, 1, function(x) sum(x == 0)))
        
        for (i in seq(nrow(x))) {
            xi <- x[i, ]
            rxi <- sum(xi == 0)

            if (rxi > 0 & ifelse(partial, rxi > rmin, TRUE)) {
                wxi <- which(xi == 0)
                if (partial) {
                    combs <- combnk(rxi, rxi - rmin)
                    for (col in seq(ncol(combs))) {
                        wxic <- wxi[combs[, col]]
                        rest <- getMatrix(noflevels[wxic]) + 1
                        basemat <- matrix(rep(xi[-wxic], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                        resmat <- cbind(basemat, rest)[, order(c(seq(ncol(x))[-wxic], wxic)), drop = FALSE]
                        result <- rbind(result, resmat)
                    }
                }
                else {
                    rest <- getMatrix(noflevels[wxi]) + 1
                    basemat <- matrix(rep(xi[-wxi], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                    resmat <- cbind(basemat, rest)[, order(c(seq(ncol(x))[-wxi], wxi)), drop = FALSE]
                    result <- rbind(result, resmat)
                }
            }
            else {
                result <- rbind(result, xi)
            }
        }

        colnames(result) <- colnames(x)
        if (length(zeroc) > 0 & partial) {
            for (i in zeroc) {
                result <- cbind(result, 0)
            }
            result <- result[, order(c(seq(ncol(result))[-zeroc], zeroc)), drop = FALSE]
            colnames(result)[zeroc] <- names(zeroc)
        }

        return(unique(result))
    }
    
    if (is.character(expression)) {

        if (length(expression) > 1) {
            expression <- expression[1]
        }

        if (identical(snames, "")) {
            syscalls <- unlist(lapply(sys.calls(), deparse))
            if (any(withdata <- grepl("with\\(", syscalls))) {
                data <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
                if (is.data.frame(data) | is.matrix(data)) {
                    snames <- colnames(data)
                }
            }
        }

        snames <- splitstr(snames)
        multivalue <- any(grepl("\\[|\\]|\\{|\\}", expression))
        
        if (multivalue) {
            expression <- gsub("[*]", "", expression)
            # return(list(expression = expression, snames = snames, noflevels = noflevels))
            checkMV(expression, snames = snames, noflevels = noflevels) # , data = data)
        }
        
        if (!grepl("[+]", expression) & grepl("[,]", expression)) {
            
            if (multivalue) {
                values <- squareBrackets(expression)
                atvalues <- paste("@", seq(length(values)), sep = "")
                for (i in seq(length(values))) {
                    expression <- gsub(values[i], atvalues[i], expression)
                }
                expression <- gsub(",", "+", expression)
                for (i in seq(length(values))) {
                    expression <- gsub(atvalues[i], values[i], expression)
                }
            }
            else {
                oldway <- unlist(strsplit(gsub("[-|;|,|[:space:]]", "", expression), split = ""))
                if (!possibleNumeric(oldway) & length(oldway) > 0) {
                    expression <- gsub(",", "+", expression)
                }
            }
        }
        
        if (any(grepl("[(|)]", expression))) {
            bl <- expandBrackets(expression, snames = snames, noflevels = noflevels)
        }
        else {
            bl <- expression
        }
        
        if (identical(bl, "")) {
            return(classify("", "admisc_simplify"))
        }

        tlist <- list(expression = bl, snames = snames)

        if (!is.null(noflevels)) {
            tlist$noflevels <- noflevels
        }
        
        bl <- tryCatch(do.call(translate, tlist), error = function(e) e)

        if (is.list(bl)) {
            return(classify("", "admisc_simplify"))
        }

        expression <- matrix(nrow = 0, ncol = ncol(bl))
        colnames(expression) <- colnames(bl)

        for (i in seq(nrow(bl))) {
            expression <- rbind(expression, as.matrix(expand.grid(lapply(bl[i, ], function(x) {
                asNumeric(splitstr(x)) + 1
            }))))
        }
        
    }
    else if (!is.matrix(expression)) {
        cat(enter)
        stop(simpleError(paste0("The input should be either a character expression or a matrix.", enter, enter)))
    }

    if (is.null(noflevels)) noflevels <- rep(2, ncol(expression))
    
    # return(list(x = remred(expression), noflevels = noflevels, partial = partial))
    expression <- dnf(remred(expression), noflevels = noflevels, partial = partial)

    if (implicants) {
        for (i in seq(ncol(expression), 1)) {
            expression <- expression[order(expression[, i]), , drop = FALSE]
        }
        rownames(expression) <- NULL
        
        return(expression)
    }

    if (is.null(colnames(expression))) {
        cat(enter)
        stop(simpleError(paste0("The input matrix should have column names.", enter, enter)))
    }

    scollapse <- scollapse | any(nchar(snames) > 1)
    expression <- writePrimeimp(expression, multivalue, collapse = ifelse(scollapse, "*", ""))
    expression <- paste(expression, collapse = " + ")

    return(classify(expression, "admisc_simplify"))
}
