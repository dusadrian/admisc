`trimstr` <- function(x, what = " ", side = "both") {
    if (is.element(what, c("*", "+"))) what <- paste("\\", what, sep = "")
    what <- ifelse(what == " ", "[[:space:]]", what)
    pattern <- switch(side,
    both = paste("^", what, "+|", what, "+$", sep = ""),
    left = paste("^", what, "+", sep = ""),
    right = paste(what, "+$", sep = "")
    )
    gsub(pattern, "", x)
}


`splitstr` <- function(x) {
    
    if (identical(x, "")) return(x)

    y <- trimstr(gsub("\\n", "", unlist(strsplit(x, split = ","))))
    
    if (any(grepl(",", x) & grepl("[{]", x))) {
        i <- 1
        while (i <= length(y)) {
            if (grepl("[{]", y[i]) & !grepl("[}]", y[i])) {
                y[i] <- paste(y[i], y[i + 1], sep = ",")
                y <- y[-(i + 1)]
            }
            i <- i + 1
        }
    }
    
    if (length(y) == 1) {
        # try again, using a semicolon
        y <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", y), split = ";")))
    }
    

    metacall <- match.call()$x

    # Provides functionality for package QCA
    if (metacall == "sort.by") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(gsub("[[:space:]]", "", y), split = "=")))
            values <- y[, 2] == TRUE
            names(values) <- y[, 1]
        }
        else {
            values <- !grepl("[+]", y)
            names(values) <- gsub("[+|-]", "", y)
        }
        return(values)
    }
    else if (metacall == "decreasing") {
        return(as.logical(y))
    }
    else if (metacall == "thresholds") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(gsub("[[:space:]]", "", y), split = "=")))
            values <- y[, 2]
            if (possibleNumeric(values)) {
                values <- asNumeric(values)
            }
            names(values) <- y[, 1]
        }
        else {
            if (possibleNumeric(y)) {
                values <- asNumeric(y)
            }
        }
        return(values)
    }
    else {
        if (possibleNumeric(y)) {
            y <- asNumeric(y)
        }
        
        return(y)
    }
}


`splitMainComponents` <- function(expression) {

    expression <- gsub("[[:space:]]", "", expression)
    
    ind.char <- unlist(strsplit(expression, split = ""))
    
    if (grepl("\\(", expression)) {
        # split the string in individual characters
    
        open.brackets <- which(ind.char == "(")
        closed.brackets <- which(ind.char == ")")
        
        invalid <- ifelse(grepl("\\)", expression), length(open.brackets) != length(closed.brackets), TRUE)
        
        if (invalid) {
            stopError("Invalid expression, open bracket \"(\" not closed with \")\".")
        }
        
        all.brackets <- sort(c(open.brackets, closed.brackets))
        
        if (length(all.brackets) > 2) {
            for (i in seq(3, length(all.brackets))) {
                if (all.brackets[i] - all.brackets[i - 1] == 1) {
                    open.brackets <- setdiff(open.brackets, all.brackets[seq(i - 1, i)])
                    closed.brackets <- setdiff(closed.brackets, all.brackets[seq(i - 1, i)])
                }
                
                if (all.brackets[i] - all.brackets[i - 1] == 2) {
                    if (ind.char[all.brackets[i] - 1] != "+") {
                        open.brackets <- setdiff(open.brackets, all.brackets[seq(i - 1, i)])
                        closed.brackets <- setdiff(closed.brackets, all.brackets[seq(i - 1, i)])
                    }
                }
            }
        }
        
        for (i in seq(length(open.brackets))) {
            plus.signs <- which(ind.char == "+")
            last.plus.sign <- plus.signs[plus.signs < open.brackets[i]]
            if (length(last.plus.sign) > 0) {
                open.brackets[i] <- max(last.plus.sign) + 1
            }
            else {
                if (1 == 1) { # ????
                    open.brackets[i] <- 1
                }
            }
            next.plus.sign <- plus.signs[plus.signs > closed.brackets[i]]
            if(length(next.plus.sign) > 0) {
                closed.brackets[i] <- min(next.plus.sign) - 1
            }
            else {
                closed.brackets[i] <- length(ind.char)
            }
        }
                    
        # create an empty list with at least 3 times as many components as number of open brackets (just to make sure I have enough)
        big.list <- vector(mode = "list", length = length(open.brackets) + 2)
        
        if (length(open.brackets) == 1) {
            # there is only one open bracket
            if (open.brackets > 1) {
                # there's something before that open bracket
                big.list[[1]] <- paste(ind.char[seq(1, open.brackets - 2)], collapse = "")
            }
            nep <- min(which(unlist(lapply(big.list, is.null))))
            big.list[[nep]] <- paste(ind.char[seq(open.brackets, closed.brackets)], collapse = "")
            if (closed.brackets < length(ind.char)) {
                # there is something beyond the closed bracket
                nep <- min(which(unlist(lapply(big.list, is.null))))
                big.list[[nep]] <- paste(ind.char[seq(closed.brackets + 2, length(ind.char))], collapse = "")
            }
        }
        else {
            for (i in seq(length(open.brackets))) {
                if (i == 1) {
                    # check if there's anything meaningful before the FIRST bracket
                    # i.e. containing a "+" sign, like "A + B(C + D)"
                    # before the first bracket is "A + B", but only B should be multiplied with "C + D"
                    
                    if (open.brackets[1] > 1) {
                        # there is something before the first bracket
                        big.list[[1]] <- paste(ind.char[seq(1, open.brackets[1] - 2)], collapse = "")
                    }
                    
                    nep <- min(which(unlist(lapply(big.list, is.null))))
                    big.list[[nep]] <- paste(ind.char[seq(open.brackets[i], closed.brackets[i])], collapse = "")
                    
                }
                else {
                    nep <- min(which(unlist(lapply(big.list, is.null))))
                    big.list[[nep]] <- paste(ind.char[seq(open.brackets[i], closed.brackets[i])], collapse = "")
                    
                    if (i == length(closed.brackets)) {
                        if (closed.brackets[i] < length(ind.char)) {
                            # there is something beyond the last closed bracket
                            nep <- min(which(unlist(lapply(big.list, is.null))))
                    
                            big.list[[nep]] <- paste(ind.char[seq(closed.brackets[i] + 2, length(ind.char))], collapse = "")
                            
                        }
                    }
                    
                }
            }
        }
        
        nulls <- unlist(lapply(big.list, is.null))
        
        if (any(nulls)) {
            big.list <- big.list[-which(nulls)]
        }
        
        
        #### additional, to make a list containing a vector,
        #### rather than separate list components
        # big.list <- list(unlist(big.list))
        
    }
    else {
        big.list <- list(expression)
    }
    
    # names(big.list) <- expression
    
    return(big.list)
}



#####
# split each main component by separating brackets components
`splitBrackets` <- function(big.list) {
    # big.list <- as.vector(unlist(big.list))
    # result <- vector(mode="list", length = length(big.list))
    # for (i in seq(length(big.list))) {
    #     result[[i]] <- unlist(strsplit(unlist(strsplit(big.list[i], split="\\(")), split="\\)"))
    # }
    # names(result) <- big.list
    # return(result)
    return(lapply(big.list, function(x) {
        as.list(unlist(strsplit(unlist(strsplit(x, split="\\(")), split="\\)")))
    }))
}



#####
# remove individual components with single "*" signs 
`removeSingleStars` <- function(big.list) {
    return(lapply(big.list, function(x) {
        single.stars <- unlist(lapply(x, function(y) {
            return(y == "*")
        }))
        return(x[!single.stars])
    }))
}



#####
# split by "+"
`splitPluses` <- function(big.list) {
    return(lapply(big.list, function(x) {
        lapply(x, function(y) {
            plus.split <- unlist(strsplit(y, "\\+"))
            return(as.list(plus.split[plus.split != ""]))
        })
    }))
}



#####
# split by "*"
`splitStars` <- function(big.list, prod.split) {
    return(lapply(big.list, function(x) {
        lapply(x, function(y) {
            lapply(y, function(z) {
                star.split <- unlist(strsplit(z, ifelse(prod.split == "", "", paste("\\", prod.split, sep=""))))
                star.split <- star.split[star.split != ""]
                if (prod.split == "") {
                    tilda <- hastilde(star.split) & length(star.split) > 1
                    if (any(tilda)) {
                        tilda.pos <- which(tilda)
                        if (max(tilda.pos) == length(star.split)) {
                            stopError(paste("Unusual expression \"", z, "\": terminated with a \"~\" sign?", sep = ""))
                        }
                        star.split[tilda.pos + 1] <- paste("~", star.split[tilda.pos + 1], sep="")
                        star.split <- star.split[-tilda.pos]
                    }
                }
                
                return(as.list(star.split[star.split != ""]))
            })
        })
    }))
}



#####
# split by "~"
`splitTildas` <- function (big.list) {
    return(lapply(big.list, function(x) {
        lapply(x, function(y) {
            lapply(y, function(z) {
                lapply(z, function(w) {
                    if (hastilde(w)) {
                        wsplit <- unlist(strsplit(w, split = ""))
                        if (max(which(hastilde(wsplit))) > 1) {
                            stopError(paste("Unusual expression: ", w, ". Perhaps you meant \"*~\"?", sep = ""))
                        }
                        else {
                            return(c("~", notilde(w)))
                        }
                    }
                    else {
                        return(w)
                    }
                })
            })
        })
    }))
}



######
# determine if and which main components have brackets, and SOLVE them
`solveBrackets` <- function(big.list) {


    bracket.comps <- which(unlist(lapply(big.list, length)) > 1)
    
    if (length(bracket.comps) > 0) {
        for (i in bracket.comps) {
            lengths <- unlist(lapply(big.list[[i]], length))
            indexes <- expand.grid(lapply(lengths - 1, seq, from = 0)) + 1
            
            ncol.ind <- ncol(indexes)
            i.list <- vector("list", length = nrow(indexes))
            
            for (j in seq(length(i.list))) {
                i.list[[j]] <- vector("list", length = prod(dim(indexes)))
                start.position <- 1
                
                for (k in seq(ncol.ind)) {
                    for (l in seq(length(big.list[[i]][[k]][[indexes[j, k]]]))) {
                        i.list[[j]][[start.position]] <- big.list[[i]][[k]][[indexes[j, k]]][[l]]
                        start.position <- start.position + 1
                    }
                }
                
                if (start.position <= length(i.list[[j]])) {
                    i.list[[j]] <- i.list[[j]][- seq(start.position, length(i.list[[j]]))]
                }
            }
            
            
            big.list[[i]] <- list(i.list)
        }
    }
    
    return(big.list)
}



`simplifyList` <- function(big.list) {
    lengths <- unlist(lapply(big.list, function(x) length(x[[1]])))

    bl <- vector("list", length = sum(lengths))
    
    pos <- 1
    
    for (i in seq(length(big.list))) {
        for (j in seq(lengths[i])) {
            blj <- unlist(big.list[[i]][[1]][[j]])
            if (hastilde(blj[1]) & nchar(blj[1]) == 1) {
                blj <- blj[-1]
                for (b in seq(length(blj))) {
                    if (tilde1st(blj[b])) {
                        blj[b] <- notilde(blj[b])
                    }
                    else {
                        blj[b] <- paste0("~", blj[b])
                    }
                }
            }
            bl[[pos]] <- unique(blj)
            pos <- pos + 1
        }
    }

    return(unique(bl[!unlist(lapply(bl, function(x) any(duplicated(notilde(x)))))]))
}



`getNonChars` <- function(x) {
    # split by "+", incluging the trimming of the white space
    x <- gsub("^[[:space:]]+|[[:space:]]+$", "", unlist(strsplit(x, "\\+")))
    z <- vector(mode="list", length=length(x))
    for (i in seq(length(x))) {
        z[[i]] <- strsplit(gsub("[[:alnum:]]", "", x[i]), "+")[[1]]
    }
    z <- notilde(unique(unlist(z)))
    
    return(z[-which(z == "")])
}
