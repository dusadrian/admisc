`stopError` <- function(message, enter = "\n") {
    stop(simpleError(paste0(enter, message, enter, enter)))
}
