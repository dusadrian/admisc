`stopError` <- function(message, enter = "\n") {
    cat(enter)
    stop(simpleError(paste0(message, enter, enter)))
}
