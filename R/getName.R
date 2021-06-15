
`getName` <- function(x) {
    result <- rep("", length(x))
    x <- as.vector(gsub("1-", "", gsub("[[:space:]]", "", x)))
    
    for (i in seq(length(x))) {
                                        
        condsplit <- unlist(strsplit(x[i], split=""))
        
        startpos <- 0
        keycode <- ""
        
        if (any(condsplit == "]")) {
            startpos <- max(which(condsplit == "]"))
            keycode <- "]"
        }
        
        
        if (any(condsplit == "$")) {
            sp <- max(which(condsplit == "$"))
            if (sp > startpos) {
                startpos <- sp
                keycode <- "$"
            }
        }
        
        
        if (identical(keycode, "$")) {
            result[i] <- substring(x[i], startpos + 1)
        }
        else if (identical(keycode, "]")) {
            
            # keycode is "]"
            # this is a matrix or a list
            # determine where the indexing starts
            stindex <- max(which(condsplit == "["))
            filename <- paste(condsplit[seq(ifelse(any(condsplit == "("), which(condsplit == "("), 0) + 1, min(which(condsplit == "[")) - 1)], collapse="")
            
            # ptn = possibly the name
            ptn <- substr(x, stindex + 1, startpos)
            postring <- grepl("\"", ptn)
            ptn <- gsub("\"|]|,|\ ", "", ptn)
            
            # ptn <- unlist(strsplit(ptn, split=":"))
            stopindex <- ifelse(identical(condsplit[stindex - 1], "["), stindex - 2, stindex - 1)
            
            # determine if what remains is a number or a name
            if (possibleNumeric(ptn)) {
                # it's a number (an index)
                # see if it has column names
                
                # stopindex <- ifelse(identical(condsplit[stindex - 1], "["), stindex - 2, stindex - 1)
                cols <- eval.parent(parse(text = paste("colnames(", filename, ")", sep = "")))
                
                if (!is.null(cols)) {
                    result[i] <- cols[as.numeric(ptn)]
                }
            }
            else {
                # it's a name
                
                # just to make sure it's not something like "1:2"
                if (!grepl(":", ptn)) {
                    result <- ptn
                }
                
                if (!postring) { # could be something like mydf[, i] and ptn = i here
                    ptnfound <- FALSE
                    n <- 1
                    
                    if (eval.parent(parse(text = paste0("\"", ptn, "\" %in% ls()")), n = 1)) {
                        ptn <- eval.parent(parse(text = paste("get(", ptn, ")", sep = "")), n = 1)
                        ptnfound <- TRUE
                    }
                    else if (eval.parent(parse(text = paste0("\"", ptn, "\" %in% ls()")), n = 2)) {
                        ptn <- eval.parent(parse(text = paste("get(\"", ptn, "\")", sep = "")), n = 2)
                        ptnfound <- TRUE
                        n <- 2
                    }
                    
                    if (ptnfound) {
                        # check if it's a number
                        if (possibleNumeric(ptn)) {
                            result <- eval.parent(parse(text = paste("colnames(", filename, ")[", ptn, "]", sep = "")), n = n)
                        }
                        else {
                            result <- ptn
                        }
                    }
                }
            }
        }
        else {
            result <- x
        }
    }
    return(gsub(",|\ ", "", result))
}
