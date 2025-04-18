`sopos` <- function(input, snames = "", noflevels = NULL) {

    if (!is.null(noflevels)) {
        noflevels <- splitstr(noflevels)
    }

    isol <- NULL

    input <- recreate(substitute(input))
    snames <- recreate(substitute(snames))
    minimized <- methods::is(input, "QCA_min")


    if (minimized) {
        snames <- input$tt$options$conditions
        star <- any(nchar(snames) > 1)

        if (input$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
            star <- FALSE
        }

        noflevels <- input$tt$noflevels

        if (is.element("i.sol", names(input))) {

            elengths <- unlist(lapply(input$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(input$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")

            input <- unlist(lapply(input$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))

        }
        else {
            input <- unlist(lapply(input$solution, paste, collapse = " + "))
        }

        if (!star) {
            input <- gsub("[*]", "", input)
        }
        # result <- negateLoop(input)

        # attr(result, "snames") <- input$tt$options$conditions

    }

    if (methods::is(input, "admisc_deMorgan")) {
        input <- unlist(input)
    }

    if (!is.character(input)) {
        stopError("The expression should be a character vector.")
    }

    star <- any(grepl("[*]", input))

    if (!identical(snames, "")) {
        snames <- splitstr(snames)
        if (any(nchar(snames) > 1)) {
            star <- TRUE
        }
    }

    mv <- any(grepl("\\{|\\}|\\[|\\]", input))
    if (mv) start <- FALSE


    negateit <- function(x, snames = "", noflevels = NULL) {

        callist <- list(expression = x)
        if (!identical(snames, "")) callist$snames <- snames
        if (!is.null(noflevels)) callist$noflevels <- noflevels

        # if (simple) {
            # x <- do.call(simplify, callist)
        # }

        trexp <- do.call(translate, callist)

        snames <- colnames(trexp)

        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(trexp))
        }

        snoflevels <- lapply(noflevels, function(x) seq(x) - 1)

        negated <- paste(apply(trexp, 1, function(x) {
            wx <- which(x != -1) # more acurate than >= 0, now we also have multiple levels like 1,2
            x <- x[wx]
            nms <- names(x)

            x <- sapply(seq_along(x), function(i) {
                paste(setdiff(snoflevels[wx][[i]], splitstr(x[i])), collapse = ",")
            })

            if (mv) {
                return(paste("(", paste(nms, "{", x, "}", sep = "", collapse = " + "), ")", sep = ""))
            }
            else {
                nms[x == 0] <- paste("~", nms[x == 0], sep = "")
                result <- paste(nms, collapse = " + ", sep = "")
                if (length(nms) > 1) {
                    result <- paste("(", result, ")", sep = "")
                }
                return(result)
            }

        }), collapse = "*")

        return(negated)
    }

    # return(list(input = input, snames = snames, noflevels = noflevels))

    result <- lapply(input, function(x) {
        if (grepl("\\(", x)) {
            xexp <- expandBrackets(x, snames = snames, noflevels = noflevels)
            # to deal with a single pair of brackets, something like "(a + ~c)"
            # which technically does not qualify as a POS
            if (!identical(xexp, gsub("\\(|\\)", "", x))) {
                return(xexp)
            }
            x <- xexp
        }

        return(
            paste(
                unlist(lapply(x, negateit, snames = snames, noflevels = noflevels)),
                collapse = " + "
            )
        )

    })

    names(result) <- unname(input)

    if (!minimized) {
        # result <- unlist(result)
        attr(result, "expressions") <- input
    }

    if (!identical(snames, "")) {
        attr(result, "snames") <- snames
    }

    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }

    attr(result, "minimized") <- minimized

    return(classify(result, "admisc_deMorgan"))

}
