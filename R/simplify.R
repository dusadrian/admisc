`simplify` <- function(expression = "", snames = "", noflevels = NULL, ...) {
    
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    mvregexp <- "\\[|\\]|\\{|\\}"

    enter     <- if (is.element("enter",   names(dots)))   dots$enter     else "\n"
    all.sol   <- if (is.element("all.sol",   names(dots))) dots$all.sol   else FALSE
    scollapse <- if (is.element("scollapse", names(dots))) dots$scollapse else FALSE # internal collapse method

    if (identical(snames, "")) {
        syscalls <- unlist(lapply(sys.calls(), deparse))
        usingwith <- "admisc::using\\(|using\\(|with\\("
        if (any(usingdata <- grepl(usingwith, syscalls))) {
            data <- get(
                unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1],
                envir = length(syscalls) - tail(which(usingdata), 1)
            )
            
            if (is.data.frame(data) | is.matrix(data)) {
                snames <- colnames(data)
            }
        }
    }


    scollapse <- scollapse | grepl("[*]", expression)
    multivalue <- any(grepl(mvregexp, expression))
    curly <- grepl("[{]", expression)

    if (multivalue) {
        if (is.null(noflevels) | identical(snames, "")) {
            stopError("Set names and their number of levels are required to simplify multivalue expressions.")
        }
    }

# return(list(expression = expression, snames = snames, noflevels = noflevels,
#                         implicants = TRUE))
    
    implicants <- expand(expression, snames = snames, noflevels = noflevels,
                         implicants = TRUE)
    
    if (identical(unclass(implicants), "")) {
        return(implicants)
    }

    if (is.null(noflevels)) {
        noflevels <- rep(2, ncol(implicants))
    }

    version <- -1
    if (requireNamespace("QCA", quietly = TRUE)) {
        version <- compareVersion(
            packageDescription("QCA")$Version,
            "3.7"
        )
    }
    
    if (version < 0) {
        message(paste(enter, "Error: Package QCA (>= 3.7) is needed to make this work, please install it.", enter, sep = ""))
        return(invisible(character(0)))
    }
    
    dataset <- cbind(implicants - 1, 1)
    outcome <- paste(sample(LETTERS, 10), collapse = "")
    colnames(dataset)[ncol(dataset)] <- outcome

    # return(list(input = dataset, outcome = outcome, all.sol = all.sol, simplify = TRUE))
    test <- tryCatchWEM(sols <- QCA::minimize(dataset, outcome = outcome, all.sol = all.sol, simplify = TRUE))

    if (!is.null(test)) {
        if (!is.null(test$error)) {
            if (grepl("All truth table", test$error)) {
                return("")
            }
        }
    }

    scollapse <- scollapse |
                any(nchar(colnames(implicants)) > 1) |
                any(grepl(mvregexp, unlist(sols$solution)))
            # | any(grepl("[*]", unlist(sols$solution)))

    expression <- unlist(lapply(sols$solution, function(x) {
        if (!scollapse) x <- gsub("\\*", "", x)
        return(paste(x, collapse = " + "))  
    }))
    
    # just to make sure
    if (curly) {
        expression <- gsub("\\[", "\\{", expression)
        expression <- gsub("\\]", "\\}", expression)
    }
    else {
        expression <- gsub("\\{", "\\[", expression)
        expression <- gsub("\\}", "\\]", expression)
    }

    # if (all(nchar(colnames(implicants)) == 1)) {
    #     expression <- gsub("[*]", "", expression)
    # }
    if (!identical(snames, "")) {
        attr(expression, "snames") <- snames
    }

    return(classify(expression, "admisc_simplify"))
}



`sop` <- function(...) {
    .Deprecated(msg = "Function sop() is deprecated, and has been renamed to simplify()\n")
    simplify(...)
}
