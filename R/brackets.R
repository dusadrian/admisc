`insideBrackets` <- function(x, type = "{", invert = FALSE) {
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    
    tml <- which(typematrix == type, arr.ind = TRUE)[1]

    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    result <- gsub(paste("\\", tml, sep = "", collapse = "|"), "",
        regmatches(x, gregexpr(paste("\\", tml, sep = "", collapse = "[[:alnum:]|,]*"), x), invert = invert)[[1]])
    # return(trimstr(result[result != ""]))
    result <- gsub("\\*|\\+", "", unlist(strsplit(gsub("\\s+", " ", result), split = " ")))
    return(result[result != ""])
}


`outsideBrackets` <- function(x, type = "{") {
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    tml <- which(typematrix == type, arr.ind = TRUE)[1]
    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    pattern <- paste("\\", tml, sep = "", collapse = "[[:alnum:]|,]*")
    result <- gsub("\\*|\\+", "", unlist(strsplit(gsub("\\s+", " ", trimstr(gsub(pattern, " ", x))), split = " ")))
    return(result[result != ""])
}


`curlyBrackets` <- function(x, outside = FALSE) {
    # just in case it was previously split
    x <- paste(x, collapse = "+")
    
    regexp <- "\\{[[:alnum:]|,|;]+\\}"
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- gsub("\\*", "", unlist(strsplit(res, split="\\+")))
        return(res[res != ""])
    }
    else {
        return(gsub("\\{|\\}|\\*", "", res))
    }
}


`roundBrackets` <- function(x, outside = FALSE) {
    regexp <- "\\(([^)]+)\\)"
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- unlist(strsplit(res, split="\\+"))
        return(res[res != ""])
    }
    else {
        return(gsub("\\(|\\)|\\*", "", res))
    }
}


`expandBrackets` <- function(expression, snames = "", noflevels = NULL, collapse = "*") {
    
    snames <- splitstr(snames)
    multivalue <- any(grepl("[{|}]", expression))
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))

    getbl <- function(expression) {
        bl <- splitMainComponents(gsub("[[:space:]]", "", expression))
        bl <- splitBrackets(bl)
        bl <- removeSingleStars(bl)
        bl <- splitPluses(bl)
        blu <- unlist(bl)
        # to detect something like AC + B~C with no snames (it has a tilde, but not first)
        bl <- splitStars(bl, ifelse((sl | any(hastilde(blu) & !tilde1st(blu))) & !grepl("[*]", expression) & !multivalue, "", "*"))
        bl <- solveBrackets(bl)
        bl <- simplifyList(bl)
        return(bl)
    }

    
    bl <- getbl(expression)

    if (length(bl) == 0) return("")

    expressions <- translate(paste(unlist(lapply(bl, paste, collapse = collapse)), collapse = " + "), snames = snames, noflevels = noflevels)
    snames <- colnames(expressions)

    redundant <- logical(nrow(expressions))
    
    if (nrow(expressions) > 1) {
        for (i in seq(nrow(expressions) - 1)) {
            if (!redundant[i]) {
                for (j in seq(i + 1, nrow(expressions))) {
                    if (!redundant[j]) {
                        subsetrow <- checkSubset(expressions[c(i, j), , drop = FALSE], implicants = FALSE)
                        if (!is.null(subsetrow)) {
                            redundant[c(i, j)[subsetrow]] <- TRUE
                        }
                    }
                }
            }
        }

        expressions <- expressions[!redundant, , drop = FALSE]
        if (possibleNumeric(expressions)) {
            
            mat <- matrix(asNumeric(expressions) + 1, nrow = nrow(expressions))
            colnames(mat) <- colnames(expressions)
            expressions <- sortExpressions(mat) - 1
            
        }
        else {
            expressions <- expressions[order(apply(expressions, 1, function(x) sum(x < 0)), decreasing = TRUE), , drop = FALSE]
        }
    }

    
    
    # this is different from writePrimeimp() in package QCA because the entry matrix
    # is not necessarily numeric, it can contain "1,2" for instance
    expressions <- unlist(apply(expressions, 1, function(x) {
        result <- c()
        for (i in seq(length(snames))) {
            if (x[i] != -1) {
                if (multivalue) {
                    result <- c(result, paste(snames[i], "{", x[i], "}", sep = ""))
                }
                else {
                    if (x[i] == 0) {
                        result <- c(result, paste("~", snames[i], sep = ""))
                    }
                    else {
                        result <- c(result, snames[i])
                    }
                }
            }
        }
        return(paste(result, collapse = collapse))
    }))
    

    return(paste(expressions, collapse = " + "))
}
