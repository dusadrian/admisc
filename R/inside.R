#' Evaluate an Expression in a Data Environment
#'
#' Evaluate an R expression in an environment constructed from data.
#'
#' @name inside
#' @rdname inside
#' @aliases inside.list
#' @rawRd
#' \usage{
#' inside(data, expr, ...)
#'
#' \S3method{inside}{list}(data, expr, keepAttrs = TRUE, \dots)
#' }
#'
#' \arguments{
#'     \item{data}{Data to use for constructing an environment a \code{data frame}
#'         or a \code{list}.}
#'     \item{expr}{Expression to evaluate, often a \dQuote{compound} expression,
#'         i.e., of the form \preformatted{
#'             {
#'                 a <- somefun()
#'                 b <- otherfun()
#'                 .....
#'                 rm(unused1, temp)
#'             }
#'         }}
#'
#'     \item{keepAttrs}{For the \code{\link{list}} method of \code{inside()},
#'         a \code{\link{logical}} specifying if the resulting list should keep
#'         the \code{\link{attributes}} from \code{data} and have its
#'         \code{\link{names}} in the same order.  Often this is unneeded as
#'         the result is a \emph{named} list anyway, and then \code{keepAttrs =
#'         FALSE} is more efficient.}
#'     \item{...}{Arguments to be passed to (future) methods.}
#' }
#'
#' \details{
#' This is a modified version of the base R function \code{within()}, with exactly
#' the same arguments and functionality but only one fundamental difference:
#' instead of returning a modified copy of the input data, this function alters the
#' data directly.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' mt <- mtcars
#' inside(mt, hwratio <- hp/wt)
#'
#' dim(mtcars)
#'
#' dim(mt)
#' }
#'
#' \keyword{functions}
NULL
#' @export
`inside` <- function(data, expr, ...) {
    UseMethod("inside")
}

#' @export
`inside.data.frame` <- function(data, expr, ...) {
    # modified version of within.data.frame
    dataname <- deparse(substitute(data))
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    l <- as.list(e, all.names = TRUE)
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    nl <- names(l)

    if (anyDuplicated(names(data))) {
        data_names <- names(data)
        data_order <- order(data_names, seq_along(data_names))
        data_names_sorted <- data_names[data_order]
        used <- logical(length(data))
        new_items <- list()
        new_names <- character()
        j <- 1L

        for (i in seq_along(l)) {
            name <- nl[i]

            while (j <= length(data_names_sorted) && data_names_sorted[j] < name) {
                j <- j + 1L
            }

            if (j <= length(data_names_sorted) && identical(data_names_sorted[j], name)) {
                pos <- data_order[j]
                data[[pos]] <- l[[i]]
                used[pos] <- TRUE
                j <- j + 1L
            }
            else {
                new_items[[length(new_items) + 1L]] <- l[[i]]
                new_names <- c(new_names, name)
            }
        }

        data <- data[used]
        if (length(new_items) > 0) {
            data[new_names] <- new_items
        }
    } else {
        ## del: variables to *del*ete from data[]; keep non-NULL ones
        del <- setdiff(names(data), nl)
        data[nl] <- l
        data[del] <- NULL
    }

    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    } else {
        # for instance inside(obj$DF, dosomething)
        # where obj$DF is not an "object" to replace
        structure_string <- paste(capture.output(dput(data)), collapse = " ")

        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}



#' @export
`inside.list` <- function(data, expr, keepAttrs = TRUE, ...) {
    # modified version of within.list
    parent <- parent.frame()
    dataname <- deparse(substitute(data))
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr", "keepAttrs"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    if (keepAttrs) { # names() kept in original order; also other attributes
        l <- as.list(e, all.names=TRUE)
        nl <- names(l)
        del <- setdiff(names(data), nl) # variables to delete
        data[nl] <- l
        data[del] <- NULL
    } else { # (order should not matter in *named* list)
        data <- as.list(e, all.names=TRUE)
    }

    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        structure_string <- paste(capture.output(dput(data)), collapse = " ")

        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}
