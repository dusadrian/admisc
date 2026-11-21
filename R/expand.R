#' @export
`expand` <- function(expression = "", snames = "", noflevels = NULL,
    partial = FALSE, implicants = FALSE, ...) {
# `expand` <- function(x, noflevels = NULL) {

    # expression <- recreate(substitute(expression))
    # if (!identical(expression, "")) {
    #     expression <- tryCatch(eval(parse(text = expression), envir = parent.frame()),
    #         error = function(e) expression)
    # }

    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))

    dots <- list(...)

    multivalue <- FALSE

    scollapse <- ifelse(is.element("scollapse", names(dots)), dots$scollapse, FALSE) # internal collapse method
    scollapse <- scollapse | grepl("[*]", expression)

    if (!is.null(noflevels)) {
        if (is.character(noflevels) & length(noflevels) == 1) {
            noflevels <- splitstr(noflevels)
        }
    }
    `dnf` <- function(x, noflevels = NULL, partial = FALSE) {
        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(x))
        }

        # check if any column has all values equal to zero
        # which means there is no point to expand there

        zeroc <- which(apply(x, 2, function(x) all(x == 0)))
        if (length(zeroc) > 0 & partial) {
            x <- x[, -zeroc, drop = FALSE]
        }


        blocks <- vector("list", nrow(x))
        rmin <- min(apply(x, 1, function(x) sum(x == 0)))

        for (i in seq(nrow(x))) {
            xi <- x[i, ]
            rxi <- sum(xi == 0)

            if (rxi > 0 & ifelse(partial, rxi > rmin, TRUE)) {
                wxi <- which(xi == 0)
                if (partial) {
                    combs <- combnk(rxi, rxi - rmin)
                    parts <- vector("list", ncol(combs))
                    for (col in seq(ncol(combs))) {
                        wxic <- wxi[combs[, col]]
                        rest <- getMatrix(noflevels[wxic]) + 1
                        basemat <- matrix(rep(xi[-wxic], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                        resmat <- cbind(basemat, rest)[, order(c(seq(ncol(x))[-wxic], wxic)), drop = FALSE]
                        parts[[col]] <- resmat
                    }
                    blocks[[i]] <- do.call(rbind, parts)
                }
                else {
                    rest <- getMatrix(noflevels[wxi]) + 1
                    basemat <- matrix(rep(xi[-wxi], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                    resmat <- cbind(basemat, rest)[, order(c(seq(ncol(x))[-wxi], wxi)), drop = FALSE]
                    blocks[[i]] <- resmat
                }
            }
            else {
                blocks[[i]] <- x[i, , drop = FALSE]
            }
        }

        result <- do.call(rbind, blocks)
        colnames(result) <- colnames(x)
        if (length(zeroc) > 0 & partial) {
            for (i in zeroc) {
                result <- cbind(result, 0)
            }
            result <- result[, order(c(seq(ncol(result))[-zeroc], zeroc)), drop = FALSE]
            colnames(result)[zeroc] <- names(zeroc)
        }

        return(unique(result))
    }

    if (is.character(expression)) {

        if (length(expression) > 1) {
            expression <- expression[1]
        }

        if (identical(snames, "")) {
            syscalls <- unlist(lapply(sys.calls(), deparse))
            usingwith <- "admisc::using\\(|using\\(|with\\("
            if (any(usingdata <- grepl(usingwith, syscalls))) {
                data <- get(
                    unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1],
                    envir = length(syscalls) - tail(which(usingdata), 1)
                )

                if (is.data.frame(data) | is.matrix(data)) {
                    snames <- colnames(data)
                }
            }
        }

        snames <- splitstr(snames)
        multivalue <- any(grepl("\\[|\\]|\\{|\\}", expression))

        if (multivalue) {
            expression <- gsub("[*]", "", expression)
            # return(list(expression = expression, snames = snames, noflevels = noflevels))
            checkMV(expression, snames = snames, noflevels = noflevels) # , data = data)
        }

        if (!grepl("[+]", expression) & grepl("[,]", expression)) {

            if (multivalue) {
                values <- squareBrackets(expression)
                atvalues <- paste("@", seq(length(values)), sep = "")
                for (i in seq(length(values))) {
                    expression <- gsub(values[i], atvalues[i], expression)
                }
                expression <- gsub(",", "+", expression)
                for (i in seq(length(values))) {
                    expression <- gsub(atvalues[i], values[i], expression)
                }
            }
            else {
                oldway <- unlist(strsplit(gsub("[-|;|,|[:space:]]", "", expression), split = ""))
                if (!possibleNumeric(oldway) & length(oldway) > 0) {
                    expression <- gsub(",", "+", expression)
                }
            }
        }

        if (any(grepl("[(|)]", expression))) {
            bl <- expandBrackets(expression, snames = snames, noflevels = noflevels)
        }
        else {
            bl <- expression
        }

        if (identical(bl, "")) {
            return(classify("", "admisc_simplify"))
        }

        tlist <- list(expression = bl, snames = snames)

        if (!is.null(noflevels)) {
            tlist$noflevels <- noflevels
        }

        bl <- tryCatch(do.call(translate, tlist), error = function(e) e)

        if (is.list(bl)) {
            return(classify("", "admisc_simplify"))
        }

        blocks <- lapply(seq_len(nrow(bl)), function(i) {
            as.matrix(expand.grid(lapply(bl[i, ], function(x) asNumeric(splitstr(x)) + 1)))
        })
        expression <- do.call(rbind, blocks)
        colnames(expression) <- colnames(bl)

    }
    else if (!is.matrix(expression)) {
        stopError("The input should be either a character expression or a matrix.")
    }

    if (is.null(noflevels)) noflevels <- rep(2, ncol(expression))

    # return(list(x = remred(expression), noflevels = noflevels, partial = partial))
    expression <- dnf(removeRedundantTerms(expression, 0), noflevels = noflevels, partial = partial)

    if (implicants) {
        for (i in seq(ncol(expression), 1)) {
            expression <- expression[order(expression[, i]), , drop = FALSE]
        }
        rownames(expression) <- NULL

        return(expression)
    }

    if (is.null(colnames(expression))) {
        stopError("The input matrix should have column names.")
    }

    scollapse <- scollapse | any(nchar(snames) > 1)
    expression <- writePIs(expression, multivalue, collapse = ifelse(scollapse, "*", ""))
    expression <- paste(expression, collapse = " + ")
    if (!identical(snames, "")) {
        attr(expression, "snames") <- snames
    }
    return(classify(expression, "admisc_simplify"))
}
