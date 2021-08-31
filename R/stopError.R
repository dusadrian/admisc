`stopError` <- function(message, enter = "\n") {
    
    message <- unlist(strsplit(message, split = "\\n"))

    for (i in seq(length(message))) {
        message[i] <- gsub(
            "Error: ",
            ifelse(i > 1, "       ", ""),
            paste(
                strwrap(
                    paste("Error:", message[i]),
                    exdent = 7
                ),
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
