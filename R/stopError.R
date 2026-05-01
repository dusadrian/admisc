#' @export
`stopError` <- function(message, enter = "\n", ...) {

    dots <- list(...)
    
    message <- trimws(message)

    message <- unlist(
        strsplit(message, split = "\\n")
    )

    message <- message[nzchar(message)]

    message <- paste0(
        "Error: ",
        message
    )

    for (i in seq(length(message))) {
        message[i] <- gsub(
            "Error: ",
            ifelse(i > 1, "       ", ""),
            paste(
                strwrap(message[i], exdent = 7),
                collapse = "\n"
            )
        )
    }

    if (!isFALSE(dots$prenter)) {
        cat(enter)
    }

    stop(
        simpleError(
            paste0(
                paste(message, collapse = "\n"),
                enter, enter
            )
        )
    )
}
