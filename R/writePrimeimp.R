`writePrimeimp` <- function(
    impmat, mv = FALSE, collapse = "*", snames = "", curly = FALSE,
    categories = FALSE, labels = list(), ...
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

        if (categories && length(labels) > 0) {
            fnames <- names(labels)
            for (i in seq(length(labels))) {
                values <- impmat[, fnames[i]]
                pos <- nrow(impmat) * (which(snames == fnames[i]) - 1) + 1
                pos <- seq(pos, pos + length(values) - 1)[values > 0]
                chars[pos] <- labels[[i]][values[values > 0]]
            }
        }
    }
    else {
        chars <- ifelse(impmat == 1L, paste0("~", chars), chars)
        if (categories && length(labels) > 0) {
            fnames <- names(labels)
            for (i in seq(length(labels))) {
                values <- impmat[, fnames[i]]
                # print(chars[values > 0, fnames[i]])
                chars[values > 0, fnames[i]] <- labels[[i]][values[values > 0]]
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
