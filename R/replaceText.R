#' Replace text in a string
#'
#' Provides an improved method to replace strings, compared to function
#' \bold{\code{gsub}()} in package \bold{\pkg{base}}.
#'
#' @name replaceText
#' @rdname replaceText
#' @rawRd
#' \usage{
#' replaceText(
#'     expression = "", target = "", replacement = "", protect = "",
#'     boolean = FALSE, ...)
#' }
#'
#' \arguments{
#'   \item{expression}{Character string, usually a SOP - sum of products expression.}
#'   \item{target}{Character vector or a string containing the text to be replaced.}
#'   \item{replacement}{Character vector or a string containing the text to replace with.}
#'   \item{protect}{Character vector or a string containing the text to protect.}
#'   \item{boolean}{Treat characters in a boolean way, using upper and lower case letters.}
#'   \item{...}{Other arguments, from and to other functions.}
#' }
#'
#' \details{
#' If the input expression is "J*JSR", and the task is to replace "J" with "A" and "JSR" with
#' "B", function \bold{\code{gsub}()} is not very useful since the letter "J" is
#' found in multiple places, including the second target.
#'
#' This function finds the exact location(s) of each target in the input string, starting with
#' those having the largest number of characters, making sure the locations are unique. For
#' instance, the target "JSR" is found on the location from 3 to 5, while the target "J" is
#' is found on two locations 1 and 3, but 3 was already identified in the previously found
#' location for the larger target.
#'
#' In addition, this function can also deal with target strings containing spaces.
#' }
#'
#'
#' \value{
#' The original string, replacing the target text with its replacement.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#'
#' \examples{
#' replaceText("J*JSR", "J, JSR", "A, B")
#'
#' # same output, on input expresions containing spaces
#' replaceText("J*JS R", "J, JS R", "A, B")
#'
#' # works even with Boolean expressions, where lower case
#' # letters signal the absence of the causal condition
#' replaceText("DEV + urb*LIT", "DEV, URB, LIT", "A, B, C", boolean = TRUE)
#' }
#'
#' \keyword{functions}
NULL
#' @export
    replaceText <- function(
        expression, target = "", replacement = "", protect = "",
        boolean = FALSE, ...
    ) {

        dots <- list(...)
        if (!is.character(target)) {
            stopError("The argument <target> should be character.")
        }

        if (!is.character(replacement)) {
            stopError("The argument <replacement> should be character.")
        }

        if (!isTRUE(dots$checknone)) {
            if (length(target) == 1 && !isFALSE(dots$checktarget)) {
                target <- splitstr(target)
            }

            if (length(replacement) == 1) replacement <- splitstr(replacement)

            if (length(protect) == 1) protect <- splitstr(protect)
        }

        if (length(target) != length(replacement)) {
            stopError("Length of target different from the length of replacement.")
        }

        torder <- order(nchar(target), decreasing = TRUE)
        tuplow <- target[torder]
        ruplow <- replacement[torder]

        protect <- protect[order(nchar(protect), decreasing = TRUE)]

        if (
            all(target == toupper(target)) &
            all(expression != toupper(expression)) &
            !any(grepl("~", expression))
        ) {
            boolean <- TRUE
        }

        if (boolean) {
            tuplow <- rep(toupper(tuplow), each = 2)
            ruplow <- rep(toupper(ruplow), each = 2)
            tuplow[seq(2, length(tuplow), by = 2)] <- tolower(tuplow[seq(2, length(tuplow), by = 2)])
            ruplow[seq(2, length(ruplow), by = 2)] <- tolower(ruplow[seq(2, length(ruplow), by = 2)])

            torder <- order(nchar(tuplow), decreasing = TRUE)
            tuplow <- tuplow[torder]
            ruplow <- ruplow[torder]
        }

        getPositions <- function(expression, x, y = NULL, protect = NULL) {
            if (identical(x, "")) {
                return(NULL)
            }

            positions <- vector(mode = "list", length = 0)
            pos <- 0

            for (i in seq(length(x))) {
                escx <- gsub("([][{}*\\.])", "\\\\\\1", x[i]) # escape [ ] { } * and .
                locations <- gregexpr(escx, expression)[[1]]

                # temp <- gsub(x[i], paste(rep(y[i], nchar(x[i])), collapse = ""), expression)

                if (any(locations > 0)) {
                    diffs <- c()
                    for (l in seq(length(locations))) {
                        tempd <- seq(locations[l], locations[l] + nchar(x[i]) - 1)
                        if (
                            !any(
                                is.element(
                                    tempd,
                                    c(unlist(positions), unlist(protect))
                                )
                            )
                        ) {
                            diffs <- c(diffs, tempd)
                        }
                    }

                    # diffs <- setdiff(diffs, unlist(positions))

                    if (length(diffs) > 0) {
                        if (length(diffs) == 1) {
                            pos <- pos + 1
                            positions[[pos]] <- diffs
                            names(positions)[pos] <- y[i]
                        }
                        else {
                            # the same condition can be found in multiple locations of the expression
                            start <- diffs[1]
                            for (v in seq(2, length(diffs))) {
                                if ((diffs[v] - diffs[v - 1]) > 1) {
                                    pos <- pos + 1
                                    positions[[pos]] <- seq(start, diffs[v - 1])
                                    if (!is.null(y)) {
                                        names(positions)[pos] <- y[i]
                                    }
                                    start <- diffs[v]
                                }
                            }

                            pos <- pos + 1
                            positions[[pos]] <- seq(start, diffs[length(diffs)])
                            if (!is.null(y)) {
                                names(positions)[pos] <- y[i]
                            }
                        }
                    }
                }
            }

            return(positions)

        }

        posprotect <- NULL
        if (!identical(protect, "")) {
            larger <- tuplow[nchar(tuplow) > max(nchar(protect))]
            if (length(larger) > 0) {
                posprotect <- getPositions(
                    expression,
                    x = larger
                )
            }
        }

        posprotect <- getPositions(
            expression,
            x = protect,
            protect = posprotect
        )

        positions <- getPositions(
            expression,
            x = tuplow,
            y = ruplow,
            protect = posprotect
        )

        # positions <- lapply(positions, function(x) range(x))

        covered <- logical(length(positions))
        pos2 <- positions
        if (length(positions) > 1) {
            for (i in seq(length(pos2) - 1)) {
                if (!covered[i]) {
                    for (j in seq(i + 1, length(pos2))) {
                        if (!covered[j]) {
                            if (all(is.element(seq(pos2[[j]][1], pos2[[j]][length(pos2[[j]])]), seq(pos2[[i]][1], pos2[[i]][length(pos2[[i]])])))) {
                                covered[j] <- TRUE
                            }
                        }
                    }
                }
            }
        }

        positions <- positions[!covered]


        if (length(positions) > 0) {

            # sort the positions in descending order of the first character location
            # such that, if replacing from tail to start, the other positions
            # will still hold

            first <- unlist(lapply(positions, "[[", 1))
            positions <- positions[order(first, decreasing = TRUE)]

            expression <- unlist(strsplit(expression, split = ""))
            for (i in seq(length(positions))) {

                if (length(positions[[i]]) == 1) {
                    expression[positions[[i]]] <- names(positions)[i]
                }
                # else is redundant here
                if (length(positions[[i]] > 1)) {
                    start <- positions[[i]][1]
                    stop <- positions[[i]][length(positions[[i]])]

                    if (start == 1) {
                        expression <- c(names(positions)[i], expression[-seq(start, stop)])
                    }
                    else {
                        if (stop < length(expression)) {
                            expression <- c(expression[seq(start - 1)], names(positions)[i], expression[seq(stop + 1, length(expression))])
                        }
                        else {
                            expression <- c(expression[seq(start - 1)], names(positions)[i])
                        }
                    }
                }
            }

            expression <- paste(expression, collapse = "")
        }

        return(expression)
    }
