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
            if (!length(x) || identical(x, "")) return(NULL)

            occupied <- logical(nchar(expression[1L]))
            if (length(protect)) {
                for (span in protect) occupied[span$start:span$end] <- TRUE
            }
            positions <- list()
            for (i in seq_along(x)) {
                escx <- gsub("([][{}*\\.])", "\\\\\\1", x[i])
                starts <- as.integer(gregexpr(escx, expression[1L])[[1L]])
                if (starts[1L] < 0L) next
                ends <- starts + nchar(x[i]) - 1L
                if (any(occupied)) {
                    used <- c(0, cumsum(occupied))
                    keep <- used[pmin(ends, length(occupied)) + 1L] == used[starts]
                    starts <- starts[keep]
                    ends <- ends[keep]
                }
                if (!length(starts)) next

                # Adjacent matches of one target form one replacement span.
                separate <- starts[-1L] > ends[-length(ends)] + 1L
                starts <- starts[c(TRUE, separate)]
                ends <- ends[c(separate, TRUE)]
                for (j in seq_along(starts)) {
                    occupied[starts[j]:ends[j]] <- TRUE
                    positions[[length(positions) + 1L]] <- list(
                        start = starts[j], end = ends[j], value = y[i]
                    )
                }
            }
            positions
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

        if (length(positions)) {
            starts <- vapply(positions, "[[", integer(1), "start")
            spans <- positions[order(starts)]
            text <- paste(expression, collapse = "")
            ends <- vapply(spans, "[[", integer(1), "end")
            starts <- sort(starts)
            gaps <- substring(text, c(1L, ends + 1L), c(starts - 1L, nchar(text)))
            values <- vapply(spans, "[[", character(1), "value")
            expression <- paste0(c(rbind(gaps[-length(gaps)], values), tail(gaps, 1L)), collapse = "")
        }

        return(expression)
    }
