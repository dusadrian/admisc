`simplify` <- function(expression = "", snames = "", noflevels = NULL, ...) {
    
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    other.args <- list(...)
    mvregexp <- "\\[|\\]|\\{|\\}"

    enter     <- if (is.element("enter",     names(other.args))) ""                   else "\n" # internal
    all.sol   <- if (is.element("all.sol",   names(other.args))) other.args$all.sol   else FALSE
    scollapse <- if (is.element("scollapse", names(other.args))) other.args$scollapse else FALSE # internal collapse method

    if (identical(snames, "")) {
        syscalls <- unlist(lapply(sys.calls(), deparse))
        if (any(withdata <- grepl("with\\(", syscalls))) {
            data <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
            if (is.data.frame(data) | is.matrix(data)) {
                snames <- colnames(data)
            }
        }
    }


    scollapse <- scollapse | grepl("[*]", expression)
    multivalue <- any(grepl(mvregexp, expression))
    curly <- grepl("[{]", expression)

# return(list(expression = expression, snames = snames, noflevels = noflevels,
#                         enter = enter, implicants = TRUE))
    
    implicants <- expand(expression, snames = snames, noflevels = noflevels,
                        enter = enter, implicants = TRUE)
    
    if (identical(unclass(implicants), "")) {
        return(implicants)
    }

    if (is.null(noflevels)) {
        noflevels <- rep(2, ncol(implicants))
    }

    version <- NULL
    if (requireNamespace("QCA", quietly = TRUE)) {
        version <- substring(packageDescription("QCA")$Version, 1, 3)
        version <- unlist(strsplit(version, split = "\\."))
        if (version[1] < 3 | (version[1] >= 3 & version[2] < 7)) {
            version <- NULL
        }
    }
    
    if (is.null(version)) {
        message(paste(enter, "Error: Package QCA (>= 3.7) is needed to make this work, please install it.", enter, sep = ""))
        return(invisible(character(0)))
    }
    
    dataset <- cbind(implicants - 1, 1)
    outcome <- paste(sample(LETTERS, 10), collapse = "")
    colnames(dataset)[ncol(dataset)] <- outcome


    # return(list(input = dataset, outcome = outcome, all.sol = all.sol, simplify = TRUE))
    sols <- QCA::minimize(dataset, outcome = outcome, all.sol = all.sol, simplify = TRUE)
    
    scollapse <- scollapse | any(nchar(colnames(implicants)) > 1) | any(grepl(mvregexp, unlist(sols$solution))) # | any(grepl("[*]", unlist(sols$solution)))
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
