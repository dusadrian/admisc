# http://adv-r.had.co.nz/Computing-on-the-language.html
# https://developer.r-project.org/nonstandard-eval.pdf

`using` <- function(data, expr, split.by = NULL, ...) {
    UseMethod("using")
}

`using.default` <- function(data, expr, ...) {
    eval(substitute(expr), data, enclos = parent.frame())
}

`using.data.frame` <- function(data, expr, split.by = NULL, ...) {
    
    ### TODO: see evalq() from within.data.frame

    split.by <- substitute(split.by)
    sby <- all.vars(split.by)
    nsby <- all.names(split.by)

    ###------------------------------------------------------------
    ## The code below allows specifying a quoted split.by argument
    ## but I am not sure this is a good idea
    # if (is.character(split.by)) {
    #     split.by <- gsub("[[:space:]]", "", split.by)
    #     if (grepl("[&]", split.by)) {
    #         split.by <- unlist(strsplit(split.by, split = "[&]"))
    #     }
    #     else if (grepl("[+]", split.by)) {
    #         split.by <- unlist(strsplit(split.by, split = "[+]"))
    #     }
    #
    #     sby <- nsby <- split.by
    # }
    ###------------------------------------------------------------


    expr <- substitute(expr)
    vexpr <- all.vars(expr)
    vexpr <- vexpr[is.element(vexpr, names(data))]

    # capture the use of "all variables" in a formula, for instance lm(y ~ .)
    if (any(vexpr == ".")) {
        vexpr <- colnames(data)
    }

    if (length(sby) == 0) {
        # ret <- eval(expr = expr, envir = data, enclos = parent.frame())
        # class(ret) <- c("admisc_fobject", class(ret))
        # return(ret)
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }


    
    csby <- setdiff(as.character(split.by), c("c", "+", "&"))

    test <- unlist(lapply(seq(length(csby)), function(i) {
        tryCatchWEM(eval(parse(text = csby[i]), envir = data, enclos = parent.frame()))
    }))
    

    if (length(test) > 0) {
        stopError(test[1])
    }
    
    sbylist <- lapply(
        lapply(csby, function(x) {
            eval(parse(text = x), envir = data, enclos = parent.frame())
        }),
        function(x) {
            if (inherits(x, "declared") || inherits(x, "haven_labelled")) {
                na_values <- attr(x, "na_values")
                if (inherits(x, "haven_labelled")) {
                    x[is.element(x), na_values] <- NA
                }
                labels <- attr(x, "labels", exact = TRUE)
                labels <- labels[!is.element(labels, na_values)]
                uniques <- sort(unique(c(x, labels)))
                names(uniques) <- uniques
                names(uniques)[match(labels, uniques)] <- names(labels)
                attributes(x) <- NULL
                return(factor(x, levels = uniques, labels = names(uniques)))
            }
            return(as.factor(x))
        }
    )

    names(sbylist) <- csby

    # test <- unlist(lapply(sbylist, function(x) {
    #     is.factor(x) | inherits(x, "declared") | inherits(x, "haven_labelled_spss")
    # }))

    # if (sum(test) < length(test)) {
    #     stopError("Split variables should be factors or a declared / labelled objects.")
    # }

    test <- table(sapply(sbylist, length))

    if (length(test) > 1 || nrow(data) != as.numeric(names(test))) {
        stopError("Split variables do not match the number of rows in the data.")
    }

    # split by (non-missing-declared) levels
    sl <- lapply(sbylist, function(x) {

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
        
        # if (is.factor(x)) {
            return(levels(x))
        # }
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

    ###--------------------------------
    # mandatory, otherwise subset() below will take a LOT of time
    data <- data[, vexpr, drop = FALSE]
    # because it is re-creating all declared variables, all NA indexes etc.
    ###--------------------------------

    res <- vector(mode = "list", length = nrow(slexp))

    for (r in seq(nrow(slexp))) {
        selection <- rep(TRUE, nrow(data))

        for (c in seq(ncol(slexp))) {
            val <- slexp[r, c]
            x <- sbylist[[c]] # split variable
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

        if (sum(selection, na.rm = TRUE) > 0) {
            res[[r]] <- eval(
                expr = expr,
                envir = subset(data, selection),
                enclos = parent.frame()
            )
            # res[[r]] <- with(subset(data, selection), eval(expr))
        }
    }

    wt <- any(unlist(lapply(res, function(x) class(x)[1] == "w_table")))

    if (all(unlist(lapply(res, is.atomic))) & !wt) {

        classes <- unique(unlist(lapply(res, class)))
        classes <- setdiff(classes, c("integer", "double", "character", "numeric", "complex"))

        # all are vectors (e.g. from summary) but lengths can differ
        # if one subset has NAs and others not
        lengths <- unlist(lapply(res, length))
        result <- matrix(NA, nrow = length(res), ncol = max(lengths))
        
        for (i in seq(length(res))) {
            if (!is.null(res[[i]])) {
                result[i, seq(length(res[[i]]))] <- res[[i]]
            }
        }

        result[] <- coerceMode(round(result, 3))

        rownames(result) <- apply(slexp, 1, function(x) paste(x, collapse = ", "))

        if (max(lengths) == 1) {
            colnames(result) <- as.character(as.list(expr)[[1]])
        }
        else {
            colnames(result) <- names(res[[which.max(lengths)]])
        }

        res <- result
        
        class(res) <- c("admisc_fobject", "matrix")

    }
    else {
        # res is a list
        attr(res, "split") <- slexp
        class(res) <- c("admisc_fobject", class(res))
    }
    
    return(res)
}
