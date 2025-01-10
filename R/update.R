`update.character` <- function(object, ...) {
    dots <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]

    DDIwR <- eval(parse(text = "requireNamespace('DDIwR', quietly = TRUE)"))

    if (!DDIwR) {
        stopError("Package DDIwR needs to be installed.")
    }

    if (length(object) != 1) {
        stopError("The path should be a single string.")
    }

    names(Call)[1] <- "xmlfile"

    eval(parse(text = "do.call('updateCodebook', Call)"))
}