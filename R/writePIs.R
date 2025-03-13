`writePIs` <- function(
    impmat, mv = FALSE, collapse = "*", snames = "", curly = FALSE,
    use.labels = FALSE, categories = list(), ...
) {
    ### ... is to allow calls having "use dot tilde" which is now deprecated

    if (any(impmat > 2)) {
        mv <- TRUE
    }

    dots <- list(...)

    if (is.element("categorical", names(dots))) {
        use.labels <- dots$categorical
        dots$categorical <- NULL
    }

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

        if (use.labels && length(categories) > 0) {
            fnames <- names(categories)
            for (i in seq(length(categories))) {
                values <- impmat[, fnames[i]]
                pos <- nrow(impmat) * (which(snames == fnames[i]) - 1) + 1
                pos <- seq(pos, pos + length(values) - 1)[values > 0]
                chars[pos] <- categories[[i]][values[values > 0]]
            }
        }
    }
    else {
        chars <- ifelse(impmat == 1L, paste0("~", chars), chars)
        if (use.labels && length(categories) > 0) {
            fnames <- names(categories)
            for (i in seq(length(categories))) {
                values <- impmat[, fnames[i]]
                # print(chars[values > 0, fnames[i]])
                chars[values > 0, fnames[i]] <- categories[[i]][values[values > 0]]
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


`writePrimeimp` <- function(...) {
    .Deprecated(msg = "Function writePrimeimp() is deprecated, use writePIs().\n")
    writePIs(...)
}
