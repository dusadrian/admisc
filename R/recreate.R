#' Facilitate expression substitution
#'
#' Utility function based on \code{substitute()}, to recover an unquoted input.
#'
#' @name recreate
#' @rdname recreate
#' @rawRd
#' \usage{
#' recreate(x, snames = NULL, ..., mode = "expression", envir = parent.frame())
#' }
#'
#' \arguments{
#'   \item{x}{A substituted input.}
#'   \item{snames}{A character vector of set or column names.}
#'   \item{...}{Other arguments, mainly for internal use.}
#'   \item{mode}{Use \code{"expression"} for the usual recovery of expressions,
#'         or \code{"columns"} to resolve column names.}
#'   \item{envir}{In column mode, the environment in which to evaluate the
#'         input after resolving names. Defaults to the caller of \code{recreate}.
#'         A wrapper that captures a user argument with \code{substitute()}
#'         can pass its own \code{parent.frame()} here.}
#' }
#'
#' \details{
#' In column mode, a bare name matching an entry in \code{snames} stands for
#' that exact name, including spaces or punctuation. The same rule applies
#' to the arguments of \code{c()}, including nested calls to \code{c()}.
#' Other expressions, such as a subscript on a stored character vector, are
#' evaluated as written in \code{envir}. Unknown variables raise the usual
#' R evaluation error.
#'
#' This function is especially useful when users have to provide lots of quoted
#' inputs, such as the name of the columns from a data frame to be considered
#' for a particular function.
#'
#' This is actually one of the main uses of the base function
#' \bold{\code{\link[base]{substitute}()}}, but here it can be employed to also
#' detect SOP (sum of products) expressions, explained for instance in function
#' \bold{\code{\link{translate}()}}.
#'
#' Such SOP expressions are usually used in contexts of sufficieny and necessity,
#' which are indicated with the usual signs \code{->} and \code{<-}. These are
#' both allowed by the R parser, indicating standard assignment. Due to the R's
#' internal parsing system, a sufficient expression using \code{->} is automatically
#' flipped to a necessity statement \code{<-} with reversed LHS to RHS, but this
#' function is able to determine what is the expression and what is the output.
#'
#' The other necessity code \code{<=} is also recognized, but the equivalent
#' sufficiency code \code{=>} is not allowed in unquoted expressions.
#' }
#'
#' \value{
#' In expression mode, a quoted, equivalent expression or a substituted object.
#' In column mode, the evaluated value, usually a character vector of names.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \seealso{\code{\link[base]{substitute}}, \code{\link{simplify}}}
#'
#' \examples{
#' recreate(substitute(A + ~B*C))
#'
#' foo <- function(x, ...) recreate(substitute(list(...)))
#'
#' foo(arg1 = 3, arg2 = A + ~B*C)
#'
#' df <- data.frame(A = 1, B = 2, C = 3, Y = 4)
#'
#' # substitute from the global environment
#' # the result is the builtin C() function
#' res <- recreate(substitute(C))
#'
#' is.function(res) # TRUE
#'
#' # search first within the column name space from df
#' recreate(substitute(C), colnames(df))
#' # "C"
#'
#' select <- function(x) {
#'     recreate(substitute(x), colnames(df), mode = "columns", envir = parent.frame())
#' }
#' stored <- c("B", "C")
#' select(c(A, stored[2]))
#' # c("A", "C")
#'
#' # necessity well recognized
#' recreate(substitute(A <- B))
#'
#' # but sufficiency is flipped
#' recreate(substitute(A -> B))
#'
#' # more complex SOP expressions are still recovered
#' recreate(substitute(A + ~B*C -> Y))
#' }
#'
#' \keyword{functions}
NULL
#' @export
`recreate` <- function(x, snames = NULL, ..., mode = "expression", envir = parent.frame()) {

    if (identical(mode, "columns")) {
        columnNames <- function(expr) {
            if (is.symbol(expr) && is.element(as.character(expr), snames)) {
                return(as.character(expr))
            }
            if (is.call(expr) && identical(expr[[1L]], quote(c))) {
                args <- as.list(expr)
                args[-1L] <- lapply(args[-1L], columnNames)
                return(as.call(args))
            }
            return(expr)
        }

        return(eval(columnNames(x), envir))
    }
    if (!identical(mode, "expression")) {
        stopError("Choose expression or columns for <mode>.")
    }

    ## srcref is planned to arrive via three dots ... (see pof())
    ## TODO: how to make use of it, still unclear
    # nz <- nzchar(srcref[i])
    # if (length(nz) && nz && !identical(srcref[i], x[i])) {
    #     x[i] <- srcref[i]
    # }

    # is.list() can sometimes happen if recreate(list(...)) is called
    if (is.null(x) | is.logical(x) | is.character(x) | is.list(x)) return(x)

    withinobj <- function(x) {
        x <- gsub("\"|[[:space:]]", "", x)
        for (i in seq(length(x))) {

            # in unquoted expressions, at the command prompt,
            # => will never occur here because it is not allowed by the parser
            # -> is also inversed into <- by the parser

            if (!grepl("<-|->", x[i])) {
                x[i] <- gsub(">|=>|-\\.>", "->", gsub("<|<=|<\\.-", "<-", x[i]))
            }

            arrows <- c("<-", "->")
            found <- sapply(arrows, grepl, x[i])

            if (sum(found) > 0) {
                if (sum(found) > 1) {
                    stopError("Ambiguous expression, more than one relation sign.")
                }

                xs <- unlist(strsplit(x[i], split = arrows[found]))

                if (length(xs) == 2) {
                    if (all(grepl("\\*|\\+", xs))) {
                        stopError("The outcome should be a single condition.")
                    }

                    # TODO: X*Y <- A*B + C
                    if (
                        (
                            (
                                grepl("\\*|\\+", xs[2]) &
                                !grepl("\\*|\\+", xs[1])
                            ) |
                            (
                                grepl("~", ifelse(tilde1st(xs[2]), substring(xs[2], 2), xs[2])) &
                                !grepl("~", ifelse(tilde1st(xs[1]), substring(xs[1], 2), xs[1]))
                            )
                        ) &
                        which(found) == 1
                    ) {
                        # this appears to be necessity , but it is in fact sufficiency
                        # e.g. parsed as Y <- A*B + C, but originally A*B + C -> Y
                        x[i] <- paste(rev(xs), collapse = "->")
                    }
                }
            }
        }

        return(x)
    }

    # vector with c() and list with list()
    typev <- typel <- FALSE
    callx <- identical(class(x), "call")

    dx <- deparse(x)
    if (is.character(dx) && length(dx) == 2 && dx[1] == "~") {
        dx <- paste(dx, collapse = "")
    }

    if (callx) {
        typev <- is.name(x[[1]]) & identical(as.character(x[[1]]), "c")
        typel <- is.name(x[[1]]) & identical(as.character(x[[1]]), "list")
    }
    # typev <- identical(substr(dx, 1, 2), "c(")
    # typel <- identical(substr(dx, 1, 5), "list(")

    if (callx & (typev | typel)) {

        result <- dxlist <- vector(mode = "list", length = max(1, length(x) - 1))

        if (length(x) == 1) {
            # c() or unlist(list()) or unlist(result) are NULL
            if (typev) return(NULL)
            if (typel) return(list())
        }

        if (typev) {
            if (length(snames) > 0) { # since length of NULL is zero
                # c(VAR1, VAR2, VAR3)
                # simulate a data.frame environment, search for column names
                dx <- as.character(x)[-1]
                if (all(is.element(dx, snames))) {
                    return(dx)
                }
            }
        }

        for (i in seq(length(result))) {
            dxlist[[i]] <- dx <- deparse(x[[i + 1]])
            result[[i]] <- tryCatch(eval(x[[i + 1]], envir = parent.frame(n = 2)), error = function(e) {
                withinobj(dx)
            })

            if (length(snames) > 0) {
                if (all(is.element(dx, snames))) {
                    result[[i]] <- dx
                }
            }
        }

        classes <- unlist(lapply(result, class))

        if (length(unique(classes)) > 1) {
            for (i in seq(length(result))) {
                # formula: ~SURV does not give an evaluation error
                # because it is interpreted as a "formula" by R
                # function: something like C where C is a function

                if (identical(classes[i], "formula") | (identical(classes[i], "function") & typev)) {
                    result[[i]] <- withinobj(dxlist[[i]])
                }

                if (identical(classes[i], "logical") & typev & nchar(dxlist[[i]] == 1)) {
                    result[[i]] <- withinobj(dxlist[[i]])
                }

                if (identical(classes[i], "list")) {
                    # c(T, C, F) where all T, C and F are reserved names
                    if (is.element("function", unlist(lapply(result[[i]], class)))) {
                        result[[i]] <- dxlist[[i]]
                    }
                }
            }
        }

        if (typev) {
            return(unlist(result))
        }
        else if (typel) {
            names(result) <- names(x[-1])
            return(result)
        }
    }

    if (length(snames) > 0 & all(!grepl("[[:punct:]]", notilde(dx)))) {
        if (all(is.element(notilde(dx), snames))) {
            return(dx)
        }
    }

    if (identical(class(x), "<-")) {
        return(withinobj(dx))
    }

    ntdx <- dx

    # all() to prevent longer vectors
    # it should not happen but failsafe just in case
    negated <- all(tilde1st(dx) & !grepl("\\+|\\*", dx))

    if (negated) {
        ntdx <- notilde(dx)
    }

    x <- tryCatch(
        eval(
            parse(text = ntdx),
            envir = parent.frame(n = 2)
        ),
        error = function(e) {
            withinobj(dx)
        }
    )

    if (is.numeric(x)) {
        if (negated) {
            return(1 - x)
        }
        return(x)
    }

    if (identical(class(x), "formula")) {
        return(withinobj(dx))
    }

    return(x)
}
