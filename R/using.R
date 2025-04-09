# http://adv-r.had.co.nz/Computing-on-the-language.html
# https://developer.r-project.org/nonstandard-eval.pdf

`using` <- function(data, expr, split.by = NULL, ...) {
    UseMethod("using")
}

`using.default` <- function(data, expr, ...) {
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), data, enclos = parent.frame())
}

`using.data.frame` <- function(data, expr, split.by = NULL, ...) {

    if (nrow(data) == 0) {
        stopError("There are no rows in the data.")
    }

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

    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr", "split.by"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    expr <- substitute(expr)
    vexpr <- all.vars(expr)

    # capture the use of "all variables" in a formula, for instance lm(y ~ .)
    if (any(vexpr == ".")) {
        vexpr <- colnames(data)
    } else {
        vexpr <- vexpr[is.element(vexpr, colnames(data))]
    }

    if (length(sby) == 0) {
        # ret <- eval(expr = expr, envir = data, enclos = parent.frame())
        # class(ret) <- c("admisc_fobject", class(ret))
        # return(ret)
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }

    nms <- names(data)
    existing <- sapply(sby, function(x) {
        is.element(x, nms) || exists(x, envir = parent.frame(), inherits = TRUE)
    })

    if (any(!existing)) {
        stopError("Split by variables do not exist in the data.")
    }

    sbylist <- lapply(
        lapply(sby, function(x) {
            eval(parse(text = x), envir = data, enclos = parent.frame())
        }),
        function(x) {
            if (inherits(x, "declared") || inherits(x, "haven_labelled")) {
                labels <- attr(x, "labels", exact = TRUE)
                na_values <- attr(x, "na_values")
                na_range <- attr(x, "na_range")

                if (!is.null(na_range)) {
                    if (length(na_range) > 2) {
                        stopError("Split by variable has a missing range with more than two values.")
                    }

                    na_values <- sort(union(
                        na_values,
                        seq(na_range[1], na_range[2])
                    ))
                }

                if (inherits(x, "haven_labelled")) {
                    x[is.element(x), na_values] <- NA
                }

                uniques <- sort(
                    setdiff(
                        c(undeclareit(x, drop = TRUE), labels),
                        na_values
                    )
                )
                names(uniques) <- uniques
                labels <- labels[is.element(labels, uniques)]
                names(uniques)[match(labels, uniques)] <- names(labels)
                attributes(x) <- NULL
                return(factor(x, levels = uniques, labels = names(uniques)))
            }

            return(as.factor(x))
        }
    )

    names(sbylist) <- sby

    test <- table(sapply(sbylist, length))

    if (length(test) > 1 || nrow(data) != as.numeric(names(test))) {
        stopError("Split variables do not match the number of rows in the data.")
    }

    # split by (non-missing-declared) levels
    sl <- lapply(sbylist, function(x) levels(x))

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

    empty <- sapply(res, is.null)

    res <- res[!empty]

    any_wtable <- any(
        sapply(res, function(x) class(x)[1] == "wtable" | class(x)[1] == "w_table")
    )

    slexp <- slexp[!empty, ]

    if (all(sapply(res, is.atomic)) & !any_wtable) {

        classes <- unique(unlist(lapply(res, class)))
        classes <- setdiff(classes, c("integer", "double", "character", "numeric", "complex"))

        # all are vectors (e.g. from summary) but lengths can differ
        # if one subset has NAs and others not
        lengths <- sapply(res, length)
        result <- matrix(NA, nrow = length(res), ncol = max(lengths))

        for (i in seq(length(res))) {
            if (!is.null(res[[i]])) {
                result[i, seq(length(res[[i]]))] <- res[[i]]
            }
        }

        result[] <- coerceMode(round(result, 3))
        if (is.matrix(slexp)) {
            rownames(result) <- apply(slexp, 1, function(x) paste(x, collapse = ","))
        } else {
            rownames(result) <- slexp
        }

        expr <- as.list(expr)

        if (max(lengths) == 1) {
            colnames(result) <- as.character(expr[[1]])
        }
        else {
            # something like:
            # using(aa, c(mean(A), sd(A)), split.by = group)
            if (as.character(expr[1]) == "c") {
                expr <- expr[-1]
            }

            cexpr <- sapply(expr, as.character)

            if (is.matrix(cexpr) && nrow(cexpr) == 2) {
                if (length(unique(cexpr[1, ])) == 1) {
                    cexpr <- cexpr[2, ]
                } else if (length(unique(cexpr[2, ])) == 1) {
                    cexpr <- cexpr[1, ]
                }
            }

            nms <- names(res[[which.max(lengths)]])

            if (is.null(nms)) {
                if (max(lengths) == length(expr) && !is.element("table", expr)) {
                    if (max(lengths) == length(cexpr)) {
                        nms <- cexpr
                    } else {
                        nms <- sapply(expr, deparse)
                    }
                } else {
                    nms <- rep(" ", max(lengths))
                }
            }

            # something like:
            # using(aa, c(mean(A), sd(A), summary(A)), split.by = group)
            if (
                any(nms == "") &&
                is.element("summary", cexpr) &&
                sum(nms == "") == length(expr) - 1
            ) {
                nms[nms == ""] <- setdiff(cexpr, "summary")
            }

            colnames(result) <- nms
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

