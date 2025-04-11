`recode` <- function(x, rules = NULL, cut = NULL, values = NULL, ...) {
    UseMethod("recode")
}


`recode.declared` <- function(x, rules = NULL, cut = NULL, values = NULL, ...) {

    dots <- list(...)
    na_index <- attr(x, "na_index")
    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    xlabels <- attr(x, "labels", exact = TRUE)
    attributes(x) <- NULL

    labels <- splitstr(dots[["labels"]])
    label <- dots[["label"]]

    x <- recode(x = x, rules = rules, cut = cut, values = values)

    if (is.null(names(labels))) {
        values <- sort(unique(x))
        if (length(values) == length(labels)) {
            names(values) <- labels
            labels <- values
        } else {
            stopError("The number of labels should be equal to the number of recodings.")
        }
    }

    if (is.null(na_index)) { # because of drop_na() for instance
        xlabels <- NULL
    } else {
        attr(x, "na_index") <- na_index
        attr(x, "na_values") <- na_values
        attr(x, "na_range") <- na_range
    }

    if (!is.null(xlabels)) {
        if (!is.null(na_values)) {
            xlabels <- xlabels[is.element(xlabels, na_values)]
        }
        else if (!is.null(na_range)) {
            xlabels <- xlabels[xlabels >= na_range[1] & xlabels <= na_range[2]]
        }
    }

    attr(x, "labels") <- c(labels, xlabels)
    attr(x, "label") <- label
    class(x) <- c("declared", class(x))

    return(x)
}


`recode.default` <- function(x, rules = NULL, cut = NULL, values = NULL, ...) {

    if (missing(x)) {
        stopError("Argument 'x' is missing.")
    }

    if (!is.atomic(x))   {
        stopError("The input 'x' should be an atomic vector / factor.")
    }

    if (all(is.na(x))) {
        stopError("Nothing to recode, all values are missing.")
    }

    dots <- recreate(list(...))
    as.factor.result  <- isTRUE(dots$as.factor.result)
    as.numeric.result <- !isFALSE(dots$as.numeric.result)
    factor.levels     <- splitstr(dots$levels)
    factor.labels     <- splitstr(dots[["labels"]])
    factor.ordered    <- FALSE

    if (is.element("ordered", names(dots))) {
        factor.ordered <- dots$ordered
    }
    else if (is.element("ordered_result", names(dots))) {
        factor.ordered <- dots$ordered_result
    }

    if (is.element("cuts", names(dots)) & missing(cut)) {
        # backwards compatibility
        cut <- dots[["cuts"]]
    }

    if (is.logical(factor.labels)) { # something like: factor.labels = TRUE
        factor.labels <- character(0)
    }

    if (!is.null(factor.levels) || !is.null(factor.labels)) {
        as.factor.result  <- TRUE
    }

    `getFromRange` <- function(a, b, uniques, xisnumeric) {
        copya <- a
        copyb <- b

        a <- ifelse(a == "lo", uniques[1], a)
        b <- ifelse(b == "hi", uniques[length(uniques)], b)

        if (xisnumeric) {
            a <- asNumeric(a)
            b <- asNumeric(b)
            if (a > b & (copya == "lo" | copyb == "hi")) return(NULL)
        }

        seqfrom <- which(uniques == a)
        seqto <- which(uniques == b)

        temp2 <- sort(unique(c(uniques, a, b)))

        if (length(seqfrom) == 0) {
            seqfrom <- which(uniques == temp2[which(temp2 == a) + 1])
        }

        if (length(seqto) == 0) {
            seqto <- which(uniques == temp2[which(temp2 == b) - 1])
        }

        if (length(c(seqfrom, seqto)) < 2) return(NULL)

        return(seq(seqfrom, seqto))
    }

    if (is.null(cut)) {
        if (is.null(rules)) {
            stopError("At least one argument 'rules' or 'cut' should be provided.")
        }

        rules <- gsub(
            "\n|\t", "", gsub(
                "'", "", gsub(
                    ")", "", gsub(
                        "c(", "", rules, fixed = TRUE
                    )
                )
            )
        )

        if (length(rules) == 1) {
            semicolons <- gsub("[^;]", "", rules)
            equals <- gsub("[^=]", "", rules)

            if (nchar(equals) != nchar(semicolons) + 1) {
                stopError("The rules should be separated by a semicolon.")
            }

            rules <- unlist(strsplit(rules, split = ";"))
        }

        rulsplit <- strsplit(rules, split = "=")

        oldval <- trimws(sapply(rulsplit, "[", 1))
        newval <- trimws(sapply(rulsplit, "[", 2))

        if (!is.null(factor.labels)) {
            if (length(factor.labels) != length(newval)) {
                stopError("The number of labels should be equal to the number of recodings.")
            }
        }

        temp <- rep(NA, length(x))

        elsecopy <- oldval == "else" & newval == "copy"

        if (any(elsecopy)) {
            if (is.factor(x)) {
                temp <- as.character(x)
            }
            else {
                temp <- x
            }

            newval <- newval[!elsecopy]
            oldval <- oldval[!elsecopy]
        }

        newval[newval == "missing" | newval == "NA"] <- NA

        if (any(oldval == "else")) {
            if (sum(oldval == "else") > 1) {
                stopError("Too many \"else\" statements.")
            }

            # place the "else" statement as the last one, very important
            whichelse <- which(oldval == "else")
            oldval <- c(oldval[-whichelse], oldval[whichelse])
            newval <- c(newval[-whichelse], newval[whichelse])
        }

        oldval <- lapply(
            lapply(
                lapply(oldval, strsplit, split = ","),
                "[[", 1
            ),
            function(y) {
                lapply(
                    strsplit(y, split = ":"),
                    trimstr
                )
            }
        )

        newval <- trimstr(rep(newval, unlist(lapply(oldval, length))))

        # for (i in seq(length(newval))) {
        #     tc <- tryCatch(eval(parse(text = newval[[i]])), error = function(e) e)
        #     if (!(is.list(tc) && identical(names(tc), c("message", "call")))) {
        #         newval[i] <- tc
        #     }
        # }
        #
        # if (possibleNumeric(newval)) {
        #     newval <- asNumeric(newval)
        # }

        if (any(unlist(lapply(oldval, function(y) lapply(y, length))) > 2)) {
            stopError("Too many : sequence operators.")
        }


        from <- unlist(lapply(oldval, function(y) lapply(y, "[", 1)))
        to <- unlist(lapply(oldval, function(y) lapply(y, "[", 2)))

        uniques <- if(is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))

        recoded <- NULL
        xisnumeric <- possibleNumeric(uniques)

        if (xisnumeric) {
            x <- asNumeric(x) # to be safe
            uniques <- asNumeric(uniques)
        }


        for (i in seq(length(from))) {
            if (!is.na(to[i])) { # a range
                torecode <- getFromRange(from[i], to[i], uniques, xisnumeric)
                if (!is.null(torecode)) {
                    vals <- uniques[torecode]
                    temp[is.element(x, vals)] <- newval[i]
                    recoded <- c(recoded, vals)
                }

            }
            else { # a single value

                # "else" should (must?) be the last rule
                if (from[i] == "else") {
                    temp[!is.element(x, recoded)] <- newval[i]
                }
                else if (from[i] == "missing" | from[i] == "NA") {
                    temp[is.na(x)] <- newval[i]
                }
                else {
                    # if (!any(x == from[i])) {
                    #     val <- ifelse(is.na(suppressWarnings(as.numeric(from[i]))), paste("\"", from[i], "\"", sep = ""), from[i])
                    #     stopError(paste0("The value", val, "was not found."))
                    # }
                    temp[x == from[i]] <- newval[i]
                }

                recoded <- c(recoded, from[i])
            }
        }
    }
    else {

        if (length(cut) == 1 & is.character(cut)) {
            cut <- gsub(
                "\n|\t", "", gsub(
                    "'", "", gsub(
                        ")", "", gsub(
                            "c(", "", cut, fixed = TRUE
                        )
                    )
                )
            )
            cut <- trimstr(unlist(strsplit(cut, split = ",")))
            if (length(cut) == 1) {
                cut <- trimstr(unlist(strsplit(cut, split = ";")))
            }
        }

        if (possibleNumeric(cut)) {
            cut <- asNumeric(cut)
        }

        if (any(duplicated(cut))) {
            stopError("Cut values should be unique.")
        }

        if (is.null(values)) {
            values <- seq(length(cut) + 1)
        }
        else {
            if (length(values) == 1 & is.character(values)) {
                values <- gsub(
                    "\n|\t", "", gsub(
                        "'", "", gsub(
                            ")", "", gsub(
                                "c(", "", values, fixed = TRUE
                            )
                        )
                    )
                )

                values <- trimstr(unlist(strsplit(values, split = ",")))

                if (length(values) == 1) {
                    values <- trimstr(unlist(strsplit(values, split = ";")))
                }
            }

            if (length(values) == length(cut) + 1) {
                as.numeric.result <- possibleNumeric(values)
                if (as.numeric.result) {
                    values <- asNumeric(values)
                }
            }
            else {
                stopError(
                    paste0(
                        "There should be ", length(cut) + 1,
                        " values for ", length(cut), " cut value",
                        ifelse(length(cut) == 1, "", "s"), "."
                    )
                )
            }
        }

        if (!is.null(factor.labels)) {
            if (length(factor.labels) != length(values)) {
                stopError("The number of labels should be equal to the number of recodings.")
            }
        }

        if (is.factor(x)) {
            lx <- levels(x)
            minx <- lx[1]
            maxx <- lx[length(lx)]

            if (is.numeric(cut)) {
                insidex <- FALSE
            }
            else {
                insidex <- all(is.element(cut, lx))
            }
        }
        else {
            if (is.character(x) & is.numeric(cut)) {
                insidex <- FALSE
            }
            else if (is.character(x) & is.character(cut)) {
                insidex <- is.element(cut, x[!is.na(x)])
            } else {
                insidex <- cut >= min(x, na.rm = TRUE) & cut <= max(x, na.rm = TRUE)
            }
        }

        if (!all(insidex)) {
            message <- "Cut value(s) outside the input vector."
            stopError(message)
        }

        if (is.factor(x)) {
            nx <- as.numeric(x)
            nlx <- seq(length(lx))
            nc <- match(cut, lx)
            temp <- rep(values[1], length(x))
            for (i in seq(length(cut))) {
                temp[nx > nc[i]] = values[i + 1]
            }
        }
        else {
            nax <- which(is.na(x))
            temp <- rep(values[1], length(x))
            for (i in seq(length(cut))) {
                temp[x > cut[i]] = values[i + 1]
            }
            if (length(nax) > 0) {
                temp[nax] <- NA
            }
        }

        if (!is.null(factor.labels) && length(factor.labels) == 0 && is.numeric(cut)) {
            factor.labels <- values
        }
    }

    if (as.factor.result) {
        if (length(factor.levels) == 0) {
            factor.levels <- sort(unique(na.omit(temp)))
        }

        if (!is.null(factor.labels) && length(factor.labels) == 0) {
            factor.labels <- factor.levels
        }

        temp <- factor(
            temp,
            levels = factor.levels,
            labels = factor.labels,
            ordered = factor.ordered
        )
    }
    else if (as.numeric.result) {
        if (possibleNumeric(temp)) {
            temp <- asNumeric(temp)
        }

        if (!is.null(factor.labels)) {
            names(values) <- factor.labels
            attr(temp, "labels") <- values
        }
    }

    return(temp)
}
