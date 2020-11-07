`reload` <- function(package, silent = TRUE) {
    package <- recreate(substitute(package))
    unload(package)

    if (is.element(package, rownames(installed.packages()))) { # if installed
        if (silent) {
            eval(parse(text = paste("suppressMessages(library(", package, "))")))
        }
        else {
            eval(parse(text = paste("library(", package, ")")))
        }
    }
}