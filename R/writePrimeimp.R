`writePrimeimp` <-
function(mymat, mv = FALSE, collapse = "*", snames = "", ...) {
    ### ... is to allow calls having "use dot tilde" which is now deprecated
    
    if (any(mymat > 2)) {
        mv <- TRUE
    }
    
    dots <- list(...)

    if (identical(snames, "")) {
        snames <- colnames(mymat)
    }
    else {
        # ... therefore mymat needs to be transposed
        mymat <- t(mymat)
    }
    
    chars <- snames[col(mymat)]
    curly <- dots$curly
    
    if (is.null(curly)) curly <- FALSE

    if (mv) {
        chars <- matrix(paste(chars, ifelse(curly, "{", "["), mymat - 1, ifelse(curly, "}", "]"), sep = ""), nrow = nrow(mymat))
    }
    else {
        chars <- ifelse(mymat == 1L, paste0("~", chars), chars)
    }
    
    keep <- mymat > 0L
    as.vector(unlist(lapply(split(chars[keep], row(chars)[keep]), paste, collapse = collapse)))
    
}

