`change` <- function(x, ...) {
    UseMethod("change")
}

`change.default` <- function(x, ...) {
    # Nothing to do
    return(x)
}

`change.QCA_tt` <- function(x, ...) {
    
    metacall <- match.call(expand.dots = TRUE)
    callargs <- as.list(metacall[-1])

    # dots <- list(...)

    if (!requireNamespace("QCA", quietly = TRUE)) {
        enter <- ifelse(isFALSE(dots$enter), "", "\n") # internal
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
    
    calls <- sapply(callargs, is.call)

    if (any(calls)) {
        for (i in which(calls)) {
            callist <- as.list(callargs[[i]])
            if (as.character(callist[[1]]) == "findRows") {
                if (is.null(callist$obj)) {
                    callist$obj <- callargs$x
                    callargs[[i]] <- as.call(callist)
                }
            }
        }
    }


    callist <- as.list(x$call)
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
