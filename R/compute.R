#' @export
#' @noRd
`compute` <-
function(expression = "", data = NULL, separate = FALSE, ...) { # na.rm = FALSE

    # funargs <- lapply(
    #     lapply(match.call(), deparse)[-1],
    #     function(x) gsub(paste0("'|\"|[[:space:]|", "\u00a0", "]"), "", x)
    # )

    # expression <- funargs$expression
    expression <- recreate(substitute(expression))

    syscalls <- as.character(sys.calls())
    usingwith <- "admisc::using\\(|using\\(|with\\("

    if (is.null(data)) {
        if (any(usingdata <- grepl(usingwith, syscalls))) {
            dataname <- unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1]
            data <- eval.parent(parse(text = dataname, n = 1))
        }
    }

    if (!is.element(expression, colnames(data))) { # this works even if data is NULL
        # check if the expression is a single object name from the parent environment
        if (exists(expression, envir = parent.frame())) {
            # expression might be an object name such as D
            # but D might also a function in the global environment: D() so a false positive
            temp <- eval.parent(parse(text = expression, n = 1))
            if (!is.function(temp)) {
                expression <- temp
            }
        } else if (grepl("\\$", expression)) {
            # for instance: compute(foo$expression, data = bar)
            expression <- eval(parse(text = expression), envir = parent.frame())
        }

        if (!is.atomic(expression) || length(expression) > 1 || !is.character(expression)) {
            stopError("The function compute() expects a single character string for an expression.")
        }
    }

    if (grepl("<-|<=|=>|->", expression)) {
        stopError("This function is not intended to calculate parameters of fit.")
    }

    enchar <- nchar(expression)
    if (
        identical(substring(expression, 1, 2), "~(") &
        identical(substring(expression, enchar, enchar), ")")
    ) {
        expression <- paste0("1-", substring(expression, 3, enchar - 1))
    }

    negated <- identical(unname(substring(expression, 1, 2)), "1-")
    expression <- gsub("1-", "", expression)

    if (is.null(data)) {
        # argument data is still NULL, despite being checked above
        # this means compute() works with separate objects from the global environment
        # e.g. compute(DEV + URB) where both DEV and URB are separate objects
        colnms <- colnames(
            validateNames(
                notilde(expression),
                sort(eval.parent(parse(text = "ls()", n = 1)))
            )
        )

        data <- vector(mode = "list", length = length(colnms))
        for (i in seq(length(data))) {
            data[[i]] <- eval.parent(
                parse(text = sprintf("get(\"%s\")", colnms[i]), n = 1)
            )
        }

        if (length(unique(sapply(data, length))) > 1) {
            stopError("Objects should be vectors of the same length.")
        }

        names(data) <- colnms
        data <- as.data.frame(data)
    }

    multivalue <- grepl("\\{|\\}|\\[|\\]", expression)

    info <- getInfo(data)
    if (!multivalue) {
        mvsop <- mvSOP(expression, data = data, .info = info, ... = ...)
        ppm <- translate(mvsop, data = data, .info = info, retlist = TRUE)
        rownames(ppm) <- trimstr(unlist(strsplit(expression, split = "\\+")))
    }
    else {
        ppm <- translate(expression, data = data, .info = info, retlist = TRUE)
    }

    pp <- attr(ppm, "retlist")
    retain <- apply(ppm, 2, function(x) any(x >= 0))
    pp <- lapply(pp, function(x) x[retain])
    ppm <- ppm[, retain, drop = FALSE]
    data <- data[, retain, drop = FALSE]

    infodata <- if (all(retain)) info else getInfo(data)

    data <- infodata$data

    verify(data)

    # min() and max() give NA precedence over NaN when both occur in a row.
    combine <- function(left, right, fun) {
        value <- fun(left, right)
        if (anyNA(value)) {
            missing <- (is.na(left) & !is.nan(left)) | (is.na(right) & !is.nan(right))
            value[missing] <- NA_real_
        }
        value
    }

    tempList <- if (separate) vector("list", length(pp)) else NULL
    res <- NULL

    for (i in seq_along(pp)) {
        x <- which(ppm[i, ] >= 0)
        val <- pp[[i]][x]
        term <- NULL
        for (j in seq_along(val)) {
            column <- data[[colnames(ppm)[x[j]]]]
            if (!is.numeric(column) && possibleNumeric(column)) column <- asNumeric(column)
            nao <- na.omit(column)
            if (any(abs(nao - round(nao)) >= .Machine$double.eps^0.5)) {
                if (length(val[[j]]) > 1L) {
                    stopError("Multiple values specified for fuzzy data.")
                }
                if (val[[j]] == 0) column <- 1 - column
                column <- as.vector(column)
            } else {
                column <- as.numeric(is.element(column, val[[j]]))
            }
            term <- if (j == 1L) column else combine(term, column, pmin)
        }
        if (separate) tempList[[i]] <- term
        else res <- if (i == 1L) term else combine(res, term, pmax)
    }
    if (separate) {
        if (length(pp) == 1L) {
            res <- tempList[[1L]]
        } else {
            res <- as.data.frame(matrix(unlist(tempList), ncol = length(pp)))
            colnames(res) <- rownames(ppm)
        }
    }

    if (negated) res <- 1 - res
    return(res)
}
