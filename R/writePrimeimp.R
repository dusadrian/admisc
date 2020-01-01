`writePrimeimp` <-
function(mymat, mv = FALSE, collapse = "*", snames = "", ...) {
    ### ... is to allow calls having "use dot tilde" which is now deprecated
    
    if (any(mymat > 2)) {
        mv <- TRUE
    }
    
    if (identical(snames, "")) {
        snames <- colnames(mymat)
    }
    else {
        # ... therefore mymat needs to be transposed
        mymat <- t(mymat)
    }
    
    chars <- snames[col(mymat)]
    
    if (mv) {
        chars <- matrix(paste(chars, "{", mymat - 1, "}", sep = ""), nrow = nrow(mymat))
    }
    else {
        chars <- ifelse(mymat == 1L, paste0("~", chars), chars)
    }
    
    keep <- mymat > 0L
    as.vector(unlist(lapply(split(chars[keep], row(chars)[keep]), paste, collapse = collapse)))
    
}

