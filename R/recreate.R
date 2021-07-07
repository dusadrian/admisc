`recreate` <- function(x, snames = NULL) {
 
    if (is.null(x) | is.logical(x) | is.character(x)) return(x)

    within <- function(x) {
        x <- gsub("\"|[[:space:]]", "", x)
        for (i in seq(length(x))) {
            if (!grepl("<=|<-|->|=>", x[i])) {
                x[i] <- gsub(">", "->", gsub("<", "<-", x[i]))
            }

            arrows <- c("<=", "<-", "=>", "->")
            # => will never occur here because it is not allowed by the parser
            
            for (j in seq(length(arrows))) {
                xs <- unlist(strsplit(x, split = arrows[j]))
                
                if (length(xs) == 2) {
                    if (all(grepl("\\*|\\+", xs))) {
                        stopError("The outcome should be one condition (only).")
                    }

                    if (j < 3) { # necessity
                        if (grepl("\\*|\\+", xs[2])) {
                            # is in fact sufficiency
                            x[i] <- paste(xs[2], arrows[j + 2], xs[1], sep = "")
                            break
                        }
                    }
                    else { # sufficiency
                        if (grepl("\\*|\\+", xs[1])) {
                            # is in fact necessity
                            x[i] <- paste(xs[2], arrows[j - 2], xs[1], sep = "")
                            break
                        }
                    }
                }
            }
        }

        return(x)
    }
    
    # vector with c() and list with list()
    typev <- typel <- FALSE
    callx <- identical(class(x), "call")

    dx <- deparse(x)
    
    if (callx) {
        typev <- is.name(x[[1]]) & identical(as.character(x[[1]]), "c")
        typel <- is.name(x[[1]]) & identical(as.character(x[[1]]), "list")
    }
    # typev <- identical(substr(dx, 1, 2), "c(")
    # typel <- identical(substr(dx, 1, 5), "list(")

    if (callx & (typev | typel)) {
        
        result <- dxlist <- vector(mode = "list", length = max(1, length(x) - 1))

        if (length(x) == 1) {
            # c() or unlist(list()) or unlist(result) are NULL
            if (typev) return(NULL)
            if (typel) return(list())
        }
        
        if (typev) {
            if (length(snames) > 0) { # since length of NULL is zero
                # c(VAR1, VAR2, VAR3)
                # simulate a data.frame environment, search for column names
                dx <- as.character(x)[-1]
                if (all(is.element(dx, snames))) {
                    return(dx)
                }
            }
        }

        for (i in seq(length(result))) {
            dxlist[[i]] <- dx <- deparse(x[[i + 1]])
            result[[i]] <- tryCatch(eval(x[[i + 1]], envir = parent.frame(n = 2)), error = function(e) {
                within(dx)
            })
            
            if (length(snames) > 0) {
                if (all(is.element(dx, snames))) {
                    result[[i]] <- dx
                }
            }
        }

        classes <- unlist(lapply(result, class))

        if (length(unique(classes)) > 1) {
            for (i in seq(length(result))) {
                # formula: ~SURV does not give an evaluation error
                # because it is interpreted as a "formula" by R
                # function: something like C where C is a function

                if (identical(classes[i], "formula") | (identical(classes[i], "function") & typev)) {
                    result[[i]] <- within(dxlist[[i]])
                }

                if (identical(classes[i], "logical") & typev & nchar(dxlist[[i]] == 1)) {
                    result[[i]] <- within(dxlist[[i]])
                }

                if (identical(classes[i], "list")) {
                    # c(T, C, F) where all T, C and F are reserved names
                    if (is.element("function", unlist(lapply(result[[i]], class)))) {
                        result[[i]] <- dxlist[[i]]
                    } 
                }
            }
        }
        
        if (typev) {
            return(unlist(result))
        }
        else if (typel) {
            names(result) <- names(x[-1])
            return(result)
        }
    }

    if (length(snames) > 0 & all(!grepl("[[:punct:]]", notilde(dx)))) {
        if (all(is.element(notilde(dx), snames))) {
            return(dx)
        }
    }

    
    if (identical(class(x), "<-")) {
        return(within(dx))
    }

    x <- tryCatch(eval(x, envir = parent.frame(n = 2)), error = function(e) within(dx))

    if (identical(class(x), "formula")) {
        return(within(dx))
    }

    return(x)
}
