#' Generic function to change the structure of an object, function of the (changed)
#' parameters used to create it.
#'
#' A generic function that applies different altering methods for different types
#' of objects (of certain classes).
#'
#' @name change
#' @rdname change
#' @rawRd
#' \usage{
#' change(x, ...)
#' }
#'
#' \arguments{
#'     \item{x}{An object of a particular class.}
#'     \item{...}{Arguments to be passed to a specific method.}
#' }
#'
#' \details{
#' For the time being, this function is designed to change truth table objects (only).
#' Future versions will likely add class methods for different other objects.
#' }
#'
#' \value{
#'     The changed object.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' \dontrun{
#' # An example to change a QCA truth table
#' library(QCA)
#'
#' ttLF <- truthTable(LF, outcome = SURV, incl.cut = 0.8)
#' minimize(ttLF, include = "?")
#'
#' # excluding contradictory simplifying assumptions
#' minimize(
#'     change(ttLF, exclude = findRows(type = 2)),
#'     include = "?"
#' )
#' }
#' }
#'
#' \keyword{functions}
NULL
#' @export
`change` <- function(x, ...) {
    UseMethod("change")
}

#' @export
`change.default` <- function(x, ...) {
    # Nothing to do
    return(x)
}

#' @export
`change.QCA_tt` <- function(x, ...) {
    metacall <- match.call(expand.dots = TRUE)
    callargs <- as.list(metacall[-1])

    if (!requireNamespace("QCA", quietly = TRUE)) {
        enter <- ifelse(isFALSE(callargs$enter), "", "\n") # internal
        message(
            paste(
                enter,
                "Error: Package QCA is needed to change a truth table.",
                enter,
                sep = ""
            )
        )
        return(invisible(character(0)))
    }

    nullargs <- sapply(callargs, is.null)
    nullnms <- names(nullargs)[nullargs]

    if (any(nullargs)) {
        callargs <- callargs[!nullargs]
    }

    if (length(callargs) == 1 & length(nullnms) == 0) {
        return(x) # nothing to do
    }

    object <- callargs[["x"]]

    `modify` <- function(x) {
        calls <- sapply(x, is.call)
        if (any(calls)) {
            for (i in which(calls)) {
                x[[i]] <- as.call(Recall(as.list(x[[i]])))
            }
        }

        if (as.character(x[[1]]) == "findRows") {
            if (is.null(x$obj)) {
                x$obj <- object
            }
        }

        return(x)
    }

    callargs <- modify(callargs)

    # x is a truth table (an object of class "QCA_tt")
    callist <- as.list(x$call) # therefore it has a "call" component
    ttname <- as.character(callargs[["x"]])

    for (i in seq(2, length(callist))) {
        callist[[i]] <- admisc::recreate(callist[[i]])
    }

    callist$data <- x$initial.data

    if (length(callargs) > 1) {
        for (i in seq(2, length(callargs))) {
            callargs[[i]] <- admisc::recreate(callargs[[i]])
        }

        for (nm in names(callargs)[-1]) {
            callist[[nm]] <- callargs[[nm]]
        }
    }

    if (length(nullnms) > 0) {
        for (nm in nullnms) {
            callist[[nm]] <- NULL
        }
    }

    x <- do.call("truthTable", callist[-1])
    callist$data <- ttname

    x$call <- as.call(callist)

    return(x)
}
