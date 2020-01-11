`simplify` <- function(expression, snames = "", noflevels = NULL, ...) {
    
    other.args <- list(...)
    
    enter    <- ifelse(is.element("enter",   names(other.args)), "",  "\n") # internal
    all.sol  <- ifelse(is.element("all.sol", names(other.args)), other.args$all.sol, FALSE)
    scollapse <- ifelse(is.element("scollapse", names(other.args)), other.args$scollapse, FALSE) # internal collapse method
    scollapse <- scollapse | grepl("[*]", expression)

    if (identical(snames, "")) {
        syscalls <- unlist(lapply(sys.calls(), deparse))
        if (any(withdata <- grepl("with\\(", syscalls))) {
            data <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
            if (is.data.frame(data) | is.matrix(data)) {
                snames <- colnames(data)
            }
        }
    }

    multivalue <- any(grepl("[{|}]", expression))
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
    
    if (!requireNamespace("QCA", quietly = TRUE)) {
        cat(enter)
        stop("Package \"QCA\" is needed to make this work, please install it.", call. = FALSE)
    }
    
    sols <- QCA::minimize(cbind(implicants - 1, 1), simplify = TRUE, all.sol = all.sol)
    scollapse <- scollapse | any(nchar(colnames(implicants)) > 1) | any(grepl("[{]", unlist(sols$solution))) # | any(grepl("[*]", unlist(sols$solution)))
    expression <- unlist(lapply(sols$solution, function(x) {
        if (!scollapse) x <- gsub("\\*", "", x)
        return(paste(x, collapse = " + "))  
    }))
    

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
