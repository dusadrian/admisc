
`getName` <- function(x, object = FALSE) {
    result <- rep("", length(x))
    x <- as.vector(gsub("1-", "", gsub("[[:space:]]", "", x)))
                                        
    condsplit <- unlist(strsplit(x, split = ""))
    
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
        if (object) {
            return(substring(x, 1, min(which(condsplit == "$")) - 1))
        }
        
        # else
        result <- substring(x, startpos + 1)
        
    }
    else if (identical(keycode, "]")) {

        # ex. dd[,c("A","B")]

        objname <- substring(x, 1, min(which(condsplit == "[")) - 1)

        if (object) {
            return(objname)
        }
        
        # else
        # keycode is "]"
        # this is a matrix or a list
        # determine where the indexing starts
        stindex <- max(which(condsplit == "["))

        stopindex <- ifelse(
            identical(condsplit[stindex - 1], "["),
            stindex - 2,
            stindex - 1
        )
        
        # ptn = possibly the name
        ptn <- gsub("]", "", substr(x, stindex + 1, startpos)) # ",c(\"A\",\"B\")"

        if (substring(ptn, 1, 1) == ",") {
            ptn <- substring(ptn, 2)
        }

        if (substring(ptn, 1, 2) == "c(") {
            ptn <- substring(ptn, 3, nchar(ptn) - 1) # "\"A\",\"B\""
        }

        postring <- grepl("\"", ptn)
        ptn <- gsub("\"|]|\ ", "", ptn)

        ptn <- unlist(strsplit(ptn, split = ","))
        if (length(ptn) == 1) {
            # try for something like [, 1:2]
            ptn <- unlist(strsplit(ptn, split = ":"))
        }
        
        
        
        # determine if what remains is a number or a name
        if (possibleNumeric(ptn)) {
            # it's a number (an index)
            # see if it has column names
            
            cols <- eval.parent(parse(text = paste("colnames(", objname, ")", sep = "")))
            
            if (!is.null(cols)) {
                result <- cols[as.numeric(ptn)]
            }
        }
        else {
            # it's a name
            if (postring) {
                return(ptn)
            }

            # else, could be something like mydf[, i] and ptn = i here
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
                    result <- eval.parent(parse(text = paste("colnames(", objname, ")[", ptn, "]", sep = "")), n = n)
                }
                else {
                    result <- ptn
                }
            }
        }
    }
    else {
        result <- x
    }

    return(gsub(",|\ ", "", result))
}
