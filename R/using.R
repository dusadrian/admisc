# http://adv-r.had.co.nz/Computing-on-the-language.html
# https://developer.r-project.org/nonstandard-eval.pdf

`using` <- function(data, expr, split.by = NULL, ...) {

    split.by <- substitute(split.by)
    sby <- all.vars(split.by)
    nsby <- all.names(split.by)

    if (length(setdiff(nsby, c("c", "+", "&", colnames(data)))) > 0) {
        stopError("Incorrect specification of the argument <split.by>.")
    }

    expr <- substitute(expr)
    vexpr <- all.vars(expr)

    # capture the use of "all variables" in a formula, for instance lm(y ~ .)
    if (any(vexpr == ".")) {
        vexpr <- colnames(data)
    }

    data <- data[, unique(c(vexpr, sby)), drop = FALSE]
    # if (!is.null(select)) {
    #     data <- data[eval(expr = select, envir = data, enclos = parent.frame()), , drop = FALSE]
    # }

    if (length(sby) == 0) {
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }

    if (!all(is.element(sby, colnames(data)))) {
        stopError("One or more split variables not found in the data.")
    }
    
    # split by (non-missing-declared) levels
    sl <- lapply(sby, function(sb) {
        x <- data[[sb]]

        if (inherits(x, "declared") | inherits(x, "haven_labelled_spss")) {
            na_values <- attr(x, "na_values", exact = TRUE)
            labels <- attr(x, "labels", exact = TRUE)
            attributes(x) <- NULL

            x <- sort(unique(x)) # this gets rid of the NAs because of sort()
            x <- x[!is.element(x, na_values)]

            if (!is.null(labels)) {
                havelabels <- is.element(x, labels)
                x[havelabels] <- names(labels)[match(x[havelabels], labels)]
            }

            return(as.character(x))
        }
        
        if (is.factor(x)) {
            return(levels(x))
        }
        else {
            stopError(
                sprintf(
                    "The split variable %s should be a factor or a declared / labelled variable.",
                    sb
                )
            )
        }
    })


    names(sl) <- sby

    noflevels <- unlist(lapply(sl, length))
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    
    orep  <- cumprod(
        rev(
            c(rev(noflevels)[-1], 1)
        )
    )

    retmat <- sapply(seq_len(length(sl)), function(x) {
        rep.int(
            rep.int(
                seq_len(noflevels[x]),
                rep.int(mbase[x], noflevels[x])
            ),
            orep[x]
        )
    })
    
    # slexp <- expand.grid(sl, stringsAsFactors = FALSE)

    slexp <- retmat
    for (i in seq(length(sl))) {
        slexp[, i] <- sl[[i]][retmat[, i]]
    }
    
    res <- vector(mode = "list", length = nrow(slexp))

    for (r in seq(nrow(slexp))) {
        selection <- rep(TRUE, nrow(data))

        for (c in seq(ncol(slexp))) {
            val <- slexp[r, c]
            x <- data[[sby[c]]] # split variable
            attrx <- attributes(x)

            if (inherits(x, "declared") | inherits(x, "haven_labelled_spss")) {

                attributes(x) <- NULL

                na_index <- attrx[["na_index"]]
                if (!is.null(na_index)) {
                    nms <- names(na_index)
                    x[na_index] <- nms
                }
                
                labels <- attrx[["labels"]]
                if (!is.null(labels)) {
                    havelabels <- is.element(x, labels)
                    x[havelabels] <- names(labels)[match(x[havelabels], labels)]
                }
            }

            selection <- selection & (x == val)
        }

        if (sum(selection) > 0) {
            res[[r]] <- eval(
                expr = expr,
                envir = subset(data, selection),
                enclos = parent.frame()
            )
            # res[[r]] <- with(subset(data, selection), eval(expr))
        }
    }

    if (all(unlist(lapply(res, is.atomic)))) {

        # all are vectors (e.g. from summary) but lengths can differ
        # if one subset has NAs and others not
        lengths <- unlist(lapply(res, length))
        result <- matrix(NA, nrow = length(res), ncol = max(lengths))
        
        for (i in seq(length(res))) {
            if (!is.null(res[[i]])) {
                result[i, seq(length(res[[i]]))] <- res[[i]]
            }
        }
        
        for (i in seq(ncol(slexp))) {
            slexp[, i] <- format(slexp[, i], justify = "right")
        }
        rownames(result) <- apply(slexp, 1, function(x) paste(x, collapse = ", "))

        if (max(lengths) == 1) {
            colnames(result) <- as.character(as.list(expr)[[1]])
        }
        else {
            colnames(result) <- names(res[[which.max(lengths)]])
        }
        res <- result

    }
    else {
        attr(res, "split") <- slexp
    }
    
    class(res) <- "admisc_using"
    return(res)
}
