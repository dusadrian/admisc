#' Modify variables by name
#'
#' Similar to the base function \code{\link[base]{within}()}, this function
#' lets you refer to variables by name and saves your changes in the
#' supplied object.
#'
#' @name inside
#' @rdname inside
#' @aliases inside.list inside.data.frame inside.data.table inside.matrix inside.environment inside.default
#' @rawRd
#' \usage{
#' inside(data, expr, ...)
#'
#' \S3method{inside}{list}(data, expr, keepAttrs = TRUE, \dots)
#' }
#'
#' \arguments{
#'     \item{data}{A data frame (including a tibble or a \code{data.table}),
#'         a matrix with named columns, a named list, a named atomic vector,
#'         or an environment.}
#'     \item{expr}{An R expression, or several expressions enclosed in braces.}
#'     \item{keepAttrs}{For a list, keep its attributes and the original order
#'         of its elements. If these are unimportant, \code{keepAttrs = FALSE}
#'         is more efficient.}
#'     \item{...}{A single assignment can also be written as a named argument,
#'         for example \code{inside(DF, total = men + women)}.}
#' }
#'
#' \details{
#' For a data frame \code{DF}, writing \code{inside(DF, total <- men + women)}
#' adds the column \code{total} to \code{DF}. It can also be used to change
#' an existing column, or to remove one with \code{rm()}. The data frame can
#' itself be part of another object, for instance \code{inside(obj$DF, rm(total))}.
#' To calculate a result for use elsewhere, see \code{\link{using}()}.
#'
#' The same syntax works for a tibble. Even if the tibble is grouped, an
#' expression such as \code{mean(Age)} gives the mean for the whole column.
#' For a \code{data.table}, the function updates a copy and assigns it back to
#' the supplied name. A key or secondary index is kept if its columns still
#' contain the same values in the same order.
#'
#' For a matrix, the column names can be used just as in a data frame. A new
#' column can have one value for each row, or a single value for all rows.
#' The result is still a matrix, with the same row names, even with only
#' one column left or with no columns at all. Adding a character column
#' to a numeric matrix makes the whole matrix character.
#'
#' For a named vector, each name refers to one value. An assignment can
#' change that value or add a new element at the end. Both \code{rm(x)} and
#' \code{x <- NULL} remove the element called \code{x}. The other elements
#' keep their order. If a name occurs more than once, the first occurrence is used.
#' Elements with empty or missing names are kept. The usual R rules for
#' replacement apply, including those for factor levels.
#'
#' For an environment, the expression is evaluated directly there. R looks
#' for names in that environment and then in its parents. An assignment such
#' as \code{x <- NULL} gives \code{x} the value \code{NULL}. To remove
#' \code{x} altogether, use \code{rm(x)}. Changes are visible through any
#' other name for the same environment, including changes already made if a
#' later expression gives an error.
#' }
#'
#' \value{
#' The updated object, returned invisibly. For an environment, this is the
#' supplied environment itself.
#' }
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
#'
#' # add a total for each row of a matrix
#' m <- cbind(men = c(12, 9), women = c(15, 11))
#' inside(m, total <- men + women)
#' m
#'
#' # same calculation for a single area, this time using a named vector
#' counts <- c(men = 12, women = 15)
#' inside(counts, total <- men + women)
#' counts
#'
#' # keep just the total
#' inside(counts, rm(men, women))
#' counts
#'
#' # two names for the same environment
#' e <- new.env()
#' other <- e
#' inside(e, {
#'     men <- 12
#'     women <- 15
#'     total <- men + women
#'     rm(men, women)
#' })
#' other$total
#' }
#'
#' \keyword{functions}
NULL
#' @export
`inside` <- function(data, expr, ...) {
    UseMethod("inside")
}

insideExpression <- function(expr, dots) {
    if (!identical(expr, quote(expr = ))) return(expr)
    if (length(dots) != 1L || is.null(names(dots)) || !nzchar(names(dots)[1L])) {
        stopError("Missing or ambiguous expression")
    }
    call("<-", as.name(names(dots)[1L]), dots[[1L]])
}

insideAssign <- function(target, value, parent) {
    eval(call("<-", target, call("quote", value)), parent)
}

vectorBindingNames <- function(nms) {
    !is.na(nms) & nzchar(nms) & !duplicated(nms)
}

insideBindings <- function(data, code, parent, named) {
    columns <- if (all(named)) data else as.list(data)[named]
    e <- evalq(environment(), columns, parent)
    eval(code, e)
    values <- as.list(e, all.names = TRUE)
    values[!vapply(values, is.null, logical(1), USE.NAMES = FALSE)]
}

insideUpdateColumns <- function(data, code, parent) {
    original <- names(data)
    named <- !is.na(original) & nzchar(original)
    l <- insideBindings(data, code, parent, named)
    nl <- names(l)

    if (anyDuplicated(original) || !all(named)) {
        # eval() can expose several bindings with the same name. Match their
        # occurrences in order, retaining the original column positions.
        pos <- match(nl, original)
        for (name in unique(nl[duplicated(nl)])) {
            i <- which(nl == name)
            pos[i] <- which(original == name)[seq_along(i)]
        }
        for (i in which(!is.na(pos))) data[[pos[i]]] <- l[[i]]
        keep <- !named | seq_along(data) %in% pos
        if (any(!keep)) data[which(!keep)] <- NULL
        added <- is.na(pos)
        if (any(added)) {
            data[nl[added]] <- l[added]
            # Base data-frame assignment can repair existing column names.
            names(data) <- c(original[keep], nl[added])
        }
    } else {
        ## del: variables to *del*ete from data[]; keep non-NULL ones
        del <- setdiff(names(data), nl)
        data[nl] <- l
        data[del] <- NULL
    }

    data
}

#' @export
`inside.default` <- function(data, expr, ...) {
    original <- names(data)
    if (!is.atomic(data) || !is.null(dim(data)) || is.null(original)) {
        stopError("The default inside() method requires a named atomic vector.")
    }
    code <- insideExpression(substitute(expr), as.list(substitute(list(...)))[-1L])
    elements <- lapply(seq_along(data), function(i) data[[i]])
    names(elements) <- original
    named <- vectorBindingNames(original)
    values <- insideBindings(elements, code, parent.frame(), named)
    pos <- match(names(values), original)
    updated <- data
    for (i in seq_along(values)) {
        value <- values[[i]]
        if (!is.atomic(value) || length(value) != 1L || !is.null(dim(value))) {
            stopError(sprintf("Vector element '%s' must be an atomic scalar.", names(values)[i]))
        }
        if (is.na(pos[i])) {
            updated[names(values)[i]] <- value
        } else if (!identical(elements[[pos[i]]], value)) {
            updated[pos[i]] <- value
        }
    }
    removed <- which(named & !seq_along(data) %in% pos)
    if (length(removed)) updated <- updated[-removed]
    insideAssign(substitute(data), updated, parent.frame())
}

#' @export
`inside.data.frame` <- function(data, expr, ...) {
    code <- insideExpression(substitute(expr), as.list(substitute(list(...)))[-1L])
    updated <- insideUpdateColumns(data, code, parent.frame())
    insideAssign(substitute(data), updated, parent.frame())
}

#' @export
`inside.data.table` <- function(data, expr, ...) {
    code <- insideExpression(substitute(expr), as.list(substitute(list(...)))[-1L])
    updated <- insideUpdateColumns(as.data.frame(data), code, parent.frame())

    # setDT builds fresh lookup metadata. Restore only lookups whose columns
    # survived the expression with the same values and row order.
    data.table::setDT(updated)
    unchanged <- function(cols) {
        length(cols) > 0L && all(cols %in% names(updated)) &&
            all(vapply(cols, function(col) identical(data[[col]], updated[[col]]), logical(1)))
    }
    key <- data.table::key(data)
    if (unchanged(key)) data.table::setkeyv(updated, key)
    for (cols in data.table::indices(data, vectors = TRUE)) {
        if (unchanged(cols)) data.table::setindexv(updated, cols)
    }
    data.table::setattr(updated, "class", class(data))
    insideAssign(substitute(data), updated, parent.frame())
}

#' @export
`inside.environment` <- function(data, expr, ...) {
    code <- insideExpression(substitute(expr), as.list(substitute(list(...)))[-1L])
    eval(code, envir = data)
    invisible(data)
}

#' @export
`inside.matrix` <- function(data, expr, ...) {
    if (ncol(data) > 0L && is.null(colnames(data))) {
        stopError("inside() requires column names for a matrix.")
    }
    code <- insideExpression(substitute(expr), as.list(substitute(list(...)))[-1L])
    columns <- lapply(seq_len(ncol(data)), function(j) data[, j])
    names(columns) <- colnames(data)
    columns <- insideUpdateColumns(columns, code, parent.frame())

    rows <- nrow(data)
    for (j in seq_along(columns)) {
        value <- columns[[j]]
        if (!(is.atomic(value) || is.list(value) || is.expression(value)) ||
                !is.null(dim(value)) || !length(value) %in% c(1L, rows)) {
            stopError(sprintf("Matrix column '%s' must be a vector of length 1 or %d.",
                names(columns)[j], rows))
        }
        if (length(value) != rows) columns[[j]] <- rep(value, length.out = rows)
    }

    # Include the input type even when all original columns were removed.
    values <- unlist(c(list(data[0L]), columns), recursive = FALSE, use.names = FALSE)
    dn <- dimnames(data)
    dn[2L] <- list(names(columns))
    updated <- matrix(values, nrow = rows, ncol = length(columns), dimnames = dn)
    insideAssign(substitute(data), updated, parent.frame())
}

#' @export
`inside.list` <- function(data, expr, keepAttrs = TRUE, ...) {
    # modified version of within.list
    parent <- parent.frame()
    target <- substitute(data)
    e <- evalq(environment(), data, parent)
    code <- insideExpression(substitute(expr), as.list(substitute(list(...)))[-1L])
    eval(code, e)
    if (keepAttrs) { # names() kept in original order; also other attributes
        l <- as.list(e, all.names=TRUE)
        nl <- names(l)
        del <- setdiff(names(data), nl) # variables to delete
        data[nl] <- l
        data[del] <- NULL
    } else { # (order should not matter in *named* list)
        data <- as.list(e, all.names=TRUE)
    }

    insideAssign(target, data, parent)
}
