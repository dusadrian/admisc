`writePrimeimp` <- function(
    impmat, mv = FALSE, collapse = "*", snames = "", curly = FALSE,
    categorical = FALSE, categories = list(), ...
) {
    ### ... is to allow calls having "use dot tilde" which is now deprecated
    
    if (any(impmat > 2)) {
        mv <- TRUE
    }
    
    dots <- list(...)

    if (identical(snames, "")) {
        snames <- colnames(impmat)
    }
    else {
        # ... therefore impmat needs to be transposed
        impmat <- t(impmat)
    }
    
    chars <- matrix(snames[col(impmat)], nrow = nrow(impmat))
    
    if (mv) {
        chars <- matrix(
            paste(
                chars,
                ifelse(curly, "{", "["),
                impmat - 1,
                ifelse(curly, "}", "]"),
                sep = ""
            ),
            nrow = nrow(impmat)
        )
    }
    else {
        chars <- ifelse(impmat == 1L, paste0("~", chars), chars)
        if (categorical && length(categories) > 0) {
            fnames <- names(categories)
            for (i in seq(length(categories))) {
                values <- impmat[, fnames[i]]
                # print(chars[values > 0, fnames[i]])
                chars[values > 0, fnames[i]] <- names(categories[[i]])[values[values > 0]]
            }
        }
    }
    
    keep <- impmat > 0L
    return(
        as.vector(
            unlist(
                lapply(
                    split(chars[keep], row(chars)[keep]),
                    paste,
                    collapse = collapse
                )
            )
        )
    )
    
}
