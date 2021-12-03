`negate` <- function(input, snames = "", noflevels = NULL, simplify = TRUE, ...) {
    
    # TO DO: capture and error the usage of both "cD" and "D*E" in the same expression 
    
    # return(substitute(input))
    input <- recreate(substitute(input))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    scollapse <- ifelse(
        is.element("scollapse", names(dots)),
        dots$scollapse,
        FALSE
    ) # internal collapse method

    if (!is.null(noflevels)) {
        if (is.character(noflevels)) {
            noflevels <- splitstr(noflevels)
            if (possibleNumeric(noflevels)) {
                noflevels <- asNumeric(noflevels)
            }
            else {
                stopError("Invalid number of levels.")
            }
        }
    }
    
    
    isol <- NULL

    minimized <- methods::is(input, "QCA_min")
    
    if (minimized) {
        snames <- input$tt$options$conditions
        star <- any(nchar(snames) > 1)
        
        if (input$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
            star <- FALSE
        }
        
        noflevels <- input$tt$noflevels
        
        if (is.element("i.sol", names(input))) {
            
            elengths <- unlist(lapply(input$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(input$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
            
            input <- unlist(lapply(input$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))
            
        }
        else {
            input <- unlist(lapply(input$solution, paste, collapse = " + "))
        }
        
        if (!star) {
            input <- gsub("[*]", "", input)
        }
        # result <- negateLoop(input)
        
        # attr(result, "snames") <- input$tt$options$conditions
        
    }

    if (methods::is(input, "admisc_deMorgan")) {
        input <- unlist(input)
    }
    
    
    if (!is.character(input)) {
        stopError("The expression should be a character vector.")
    }
        
    star <- any(grepl("[*]", input))
    
    if (!identical(snames, "")) {
        snames <- splitstr(snames)
        if (any(nchar(snames) > 1)) {
            star <- TRUE
        }
    }
    multivalue <- any(grepl("\\[|\\]|\\{|\\}", input))
    if (multivalue) {
        start <- FALSE
        if (is.null(noflevels) | identical(snames, "")) {
            stopError(
                paste(
                    "Set names and their number of levels are required",
                    "to negate multivalue expressions."
                )
            )
        }
    }
    
    scollapse <- scollapse | any(nchar(snames) > 1) | multivalue | star
    collapse <- ifelse(scollapse, "*", "")
    
    negateit <- function(
        x, snames = "", noflevels = NULL, simplify = TRUE, collapse = "*"
    ) {

        callist <- list(expression = x)
        callist$snames <- snames
        if (!is.null(noflevels)) callist$noflevels <- noflevels
        
        # if (simple) {
            # x <- do.call(simplify, callist)
        # }
        
        trexp <- do.call(translate, callist)
        snames <- colnames(trexp)
        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(trexp))
        }
        
        snoflevels <- lapply(noflevels, function(x) seq(x) - 1)
        sr <- nrow(trexp) == 1 # single row
        negated <- paste(
            apply(trexp, 1, function(x) {
                wx <- which(x != -1) # more acurate than >= 0, now we also have multiple levels like 1,2
                x <- x[wx]
                nms <- names(x)
                
                x <- sapply(seq_along(x), function(i) {
                    paste(
                        setdiff(snoflevels[wx][[i]], splitstr(x[i])),
                        collapse = ","
                    )
                })
                
                if (multivalue) {
                    return(paste(
                        ifelse(sr | length(wx) == 1, "", "("),
                        paste(
                            nms, "[", x, "]",
                            sep = "",
                            collapse = " + "
                        ),
                        ifelse(sr | length(wx) == 1, "", ")"),
                        sep = ""
                    ))
                }
                else {
                    nms[x == 0] <- paste0("~", nms[x == 0])
                    return(paste(
                        ifelse(sr | length(wx) == 1, "", "("),
                        paste(nms, collapse = " + ", sep = ""),
                        ifelse(sr | length(wx) == 1, "", ")"),
                        sep = ""))
                }
                
            }),
            collapse = collapse
        )
        
        negated <- expandBrackets(
            negated,
            snames = snames,
            noflevels = noflevels,
            scollapse = scollapse
        )
        
        if (simplify) {
            callist$expression <- negated
            callist$scollapse <- identical(collapse, "*")
            callist$snames <- snames
            return(unclass(do.call("simplify", callist)))
        }
        
        return(negated)
    }

    # return(list(input = input, snames = snames, noflevels = noflevels, simplify = simplify, collapse = collapse))

    result <- lapply(
        input,
        negateit,
        snames = snames,
        noflevels = noflevels,
        simplify = simplify,
        collapse = collapse
    )
    
    ### probably unnecessary hack to allow package admisc being checked without package QCA
    # e.g. via simplify()
    if (any(unlist(lapply(result, length)) == 0)) {
        return(invisible(character(0)))
    }
    ###

    names(result) <- unname(input)
    
    if (!minimized) {
        # result <- unlist(result)
        attr(result, "expressions") <- input
    }
    
    if (!identical(snames, "")) {
        attr(result, "snames") <- snames
    }
    
    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }
    
    attr(result, "minimized") <- minimized
    
    return(classify(result, "admisc_deMorgan"))
}




`deMorgan` <- function(...) {
    .Deprecated(msg = "Function deMorgan() is deprecated. Use function negate() instead.\n")
    negate(...)
}
