#' Try functions to capture warnings, errors and messages.
#'
#' This function combines the base functions \bold{\code{tryCatch}()} and
#' \bold{\code{withCallingHandlers}()} for the specific purpose of capturing
#' not only  errors and warnings but messages as well.
#'
#' @name tryCatchWEM
#' @rdname tryCatchWEM
#' @rawRd
#' \usage{
#' tryCatchWEM(expr, capture = FALSE)
#' }
#'
#'
#'
#'
#'
#'
#' \arguments{
#'     \item{expr}{Expression to be evaluated.}
#'     \item{capture}{Logical, capture the visible output.}
#' }
#'
#' \details{
#' In some situations it might be important not only to test a function, but also
#' to capture everything that is written in the R console, be it an error, a warning
#' or simply a message.
#'
#' For instance package \bold{\pkg{QCA}} (version 3.4) has a Graphical User Interface
#' that simulates an R console embedded into a web based \bold{\pkg{shiny}} app.
#'
#' It is not intended to replace function \bold{\code{tryCatch}()} in any
#' way, especially not evaluating an expression before returning or exiting, it simply
#' captures everything that is printed on the console (the visible output).
#' }
#'
#'
#' \value{
#' A list, if anything would be printed on the screen, or an empty (NULL) object
#' otherwise.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \keyword{functions}
NULL
#' @export
`tryCatchWEM` <- function(expr, capture = FALSE) {
    toreturn <- list()

    # Safe muffle handler for R < 4.0.0 compatibility
    safeMuffle <- function(restart_name) {
        if (exists("tryInvokeRestart", envir = baseenv())) {
            tryInvokeRestart(restart_name)
        } else {
            restarts <- computeRestarts()
            if (any(sapply(restarts, function(r) r$name == restart_name))) {
                invokeRestart(restart_name)
            }
        }
    }

    evaluate_code <- function() {
        withVisible(withCallingHandlers(
            tryCatch(expr,
                error = function(e) {
                    toreturn$error <<- e$message
                    NULL
                },
                interrupt = function(i) {
                    toreturn$interrupted <<- TRUE
                    NULL
                }
            ),
            warning = function(w) {
                toreturn$warning <<- c(toreturn$warning, w$message)
                safeMuffle("muffleWarning")
            },
            message = function(m) {
                toreturn$message <<- paste(toreturn$message, m$message, sep = "")
                safeMuffle("muffleMessage")
            }
        ))
    }

    if (capture) {
        captured <- capture.output({
            output <- evaluate_code()
            if (output$visible && !is.null(output$value)) {
                print(output$value)
            }
        })
        if (length(captured) > 0) {
            toreturn$output <- captured
        }
        if (exists("output") && !is.null(output$value)) {
            toreturn$value <- output$value
        }
    } else {
        evaluate_code()
    }

    if (length(toreturn) > 0) {
        return(toreturn)
    }
}
