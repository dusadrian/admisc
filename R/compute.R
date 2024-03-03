`compute` <-
function(expression = "", data = NULL, separate = FALSE, ...) { # na.rm = FALSE

    expression <- recreate(substitute(expression))
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
        syscalls <- as.character(sys.calls())
        usingwith <- "admisc::using\\(|using\\(|with\\("
        if (any(usingdata <- grepl(usingwith, syscalls))) {
            dataname <- unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1]
            data <- eval.parent(parse(text = dataname, n = 1))
            # data <- get(
            #     dataname,
            #     envir = length(syscalls) - tail(which(usingdata), 1)
            # )
        }
        else {

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

            # TODO: ?model.frame

            if (length(unique(unlist(lapply(data, length)))) > 1) {
                stopError("Objects should be vectors of the same length.")
            }

            names(data) <- colnms
            data <- as.data.frame(data)
        }
    }

    multivalue <- grepl("\\{|\\}|\\[|\\]", expression)

    if (!multivalue) {
        mvsop <- mvSOP(expression, data = data, ... = ...)
        ppm <- translate(mvsop, data = data, retlist = TRUE)
        rownames(ppm) <- trimstr(unlist(strsplit(expression, split = "\\+")))
    }
    else {
        ppm <- translate(expression, data = data, retlist = TRUE)
    }

    pp <- attr(ppm, "retlist")
    retain <- apply(ppm, 2, function(x) any(x >= 0))
    pp <- lapply(pp, function(x) x[retain])
    ppm <- ppm[, retain, drop = FALSE]
    data <- data[, retain, drop = FALSE]

    infodata <- getInfo(data)

    data <- infodata$data

    verify(data)

    tempList <- vector("list", length(pp))

    for (i in seq(length(pp))) {

        x <- which(ppm[i, ] >= 0)
        val <- pp[[i]][x]

        temp <- data[, colnames(ppm)[x], drop = FALSE]

        for (j in seq(length(val))) {

            if (!is.numeric(temp[, j]) & possibleNumeric(temp[, j])) {
                temp[, j] <- asNumeric(temp[, j])
            }

            nao <- na.omit(temp[, j])

            if (any(abs(nao - round(nao)) >= .Machine$double.eps^0.5)) { # fuzzy

                if (length(val[[j]]) > 1) {
                    stopError("Multiple values specified for fuzzy data.")
                }

                if (val[[j]] == 0) {
                    temp[, j] <- 1 - temp[, j]
                }

            }
            else { # cs or mv

                temp[, j] <- as.numeric(is.element(temp[, j], val[[j]]))

            }

        }

        if (ncol(temp) > 1) {
            # temp <- as.vector(fuzzyand(temp))
            temp <- apply(temp, 1, min, na.rm = FALSE)
        }

        tempList[[i]] <- temp
    }

    res <- as.data.frame(matrix(unlist(tempList), ncol = length(tempList)))
    colnames(res) <- rownames(ppm)

    if (ncol(res) > 1) {
        if (!separate) {
            # res <- as.vector(fuzzyor(res))
            res <- apply(res, 1, max, na.rm = FALSE)
        }
    }
    else {
        res <- as.vector(res[, 1])
    }

    if (negated) res <- 1 - res

    return(res)
}
