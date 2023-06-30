
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
        # or dd[,c(A,B)] if the quotes have been removed before this function

        objname <- substring(x, 1, min(which(condsplit == "[")) - 1)

        if (object) {
            return(objname)
        }

        nms <- character(0)
        for (target in c("names", "colnames")) {
            for (n in 1:2) {
                if (length(nms) == 0) {
                    testnms <- tryCatchWEM(
                        nms <- eval.parent(
                            parse(
                                text = paste(target, "(", objname, ")", sep = "")
                            ),
                            n = n
                        )
                    )
                }
            }
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

        postring <- grepl("'|\"", ptn)
        ptn <- gsub("'|\"|]|\ ", "", ptn)

        ptn <- unlist(strsplit(ptn, split = ","))
        if (length(ptn) == 1) {
            # try for something like [, 1:2]
            ptn <- unlist(strsplit(ptn, split = ":"))
        }

        # determine if what remains is a number or a name
        if (possibleNumeric(ptn)) {
            # it's a number (an index)
            # see if it has column names
            
            if (length(nms) > 0) {
                result <- nms[as.numeric(ptn)]
            }
        }
        else {
            # it's a name
            if (postring) {
                return(ptn)
            }

            if (length(nms) > 0) {
                if (all(is.element(ptn, nms))) {
                    return(ptn)
                }
            }
        }
    }
    else {
        result <- x
    }

    return(gsub(",|\ ", "", result))
}
