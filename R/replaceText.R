
    replaceText <- function(expression, target = "", replacement = "", boolean = FALSE, ...) {

        dots <- list(...)
        enter <- ifelse (is.element("enter", names(dots)), "",  "\n")

        if (!is.character(target)) {
            stopError("The \"target\" argument should be character.")
        }

        if (!is.character(replacement)) {
            stopError("The \"replacement\" argument should be character.")
        }

        if (length(target) == 1) target <- splitstr(target)
        if (length(replacement) == 1) replacement <- splitstr(replacement)

        if (length(target) != length(replacement)) {
            stopError("Length of target different from the length of replacement.")
        }

        torder <- order(nchar(target), decreasing = TRUE)
        tuplow <- target[torder]
        ruplow <- replacement[torder]

        if (all(target == toupper(target)) & all(expression != toupper(expression)) & !any(grepl("~", expression))) {
            boolean <- TRUE
        }

        if (boolean) {
            tuplow <- rep(toupper(tuplow), each = 2)
            ruplow <- rep(toupper(ruplow), each = 2)
            tuplow[seq(2, length(tuplow), by = 2)] <- tolower(tuplow[seq(2, length(tuplow), by = 2)])
            ruplow[seq(2, length(ruplow), by = 2)] <- tolower(ruplow[seq(2, length(ruplow), by = 2)])
        }

        positions <- vector(mode = "list", length = 0)
        pos <- 0
        
        for (i in order(nchar(tuplow), decreasing = TRUE)) {
            
            locations <- gregexpr(tuplow[i], expression)[[1]]
            # temp <- gsub(tuplow[i], paste(rep(ruplow[i], nchar(tuplow[i])), collapse = ""), expression)
            
            if (any(locations > 0)) {
                diffs <- c()
                for (l in seq(length(locations))) {
                    tempd <- seq(locations[l], locations[l] + nchar(tuplow[i]) - 1)
                    if (!any(is.element(tempd, unlist(positions)))) {
                        diffs <- c(diffs, tempd)
                    }
                }
                
                # diffs <- setdiff(diffs, unlist(positions))

                if (length(diffs) > 0) {
                    if (length(diffs) == 1) {
                        pos <- pos + 1
                        positions[[pos]] <- diffs
                        names(positions)[pos] <- ruplow[i]
                    }
                    else {
                        # the same condition can be found in multiple locations of the expression
                        start <- diffs[1]
                        for (v in seq(2, length(diffs))) {
                            if ((diffs[v] - diffs[v - 1]) > 1) {
                                pos <- pos + 1
                                positions[[pos]] <- c(start, diffs[v - 1])
                                names(positions)[pos] <- ruplow[i]
                                start <- diffs[v]
                            }
                        }

                        pos <- pos + 1
                        positions[[pos]] <- c(start, diffs[length(diffs)])
                        names(positions)[pos] <- ruplow[i]
                    }
                }
            }
        }

        covered <- logical(length(positions))
        pos2 <- positions
        if (pos > 1) {
            for (i in seq(length(pos2) - 1)) {
                if (!covered[i]) {
                    for (j in seq(i + 1, length(pos2))) {
                        if (!covered[j]) {
                            if (all(is.element(seq(pos2[[j]][1], pos2[[j]][length(pos2[[j]])]), seq(pos2[[i]][1], pos2[[i]][length(pos2[[i]])])))) {
                                covered[j] <- TRUE
                            }
                        }
                    }
                }
            }
        }

        positions <- positions[!covered]


        if (length(positions) > 0) {

            # sort the positions in descending order of the first character location
            # such that, if replacing from tail to start, the other positions
            # will still hold

            first <- unlist(lapply(positions, "[[", 1))
            positions <- positions[order(first, decreasing = TRUE)]

            expression <- unlist(strsplit(expression, split = ""))
            for (i in seq(length(positions))) {

                if (length(positions[[i]]) == 1) {
                    expression[positions[[i]]] <- names(positions)[i]
                }
                # else is redundant here
                if (length(positions[[i]] > 1)) {
                    start <- positions[[i]][1]
                    stop <- positions[[i]][length(positions[[i]])]

                    if (start == 1) {
                        expression <- c(names(positions)[i], expression[-seq(start, stop)])
                    }
                    else {
                        if (stop < length(expression)) {
                            expression <- c(expression[seq(start - 1)], names(positions)[i], expression[seq(stop + 1, length(expression))])
                        }
                        else {
                            expression <- c(expression[seq(start - 1)], names(positions)[i])
                        }
                    }
                }
            }

            expression <- paste(expression, collapse = "")
        }

        return(expression)
    }
