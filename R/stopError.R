`stopError` <- function(message, enter = "\n") {
    
    message <- paste0(
        "Error: ",
        unlist(
            strsplit(message, split = "\\n")
        )
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

    cat(enter)

    stop(
        simpleError(
            paste0(
                paste(message, collapse = "\n"),
                enter, enter
            )
        )
    )
}
