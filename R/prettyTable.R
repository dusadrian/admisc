`prettyTable` <-
function(mytable) {
    
    if (methods::is(mytable, "pic")) {
        class(mytable) <- "matrix" # to remove the class "pic"
    }
    else {
        mytable <- as.matrix(mytable) # to make sure it's a matrix
    }
    
    if (is.logical(mytable)) {
        mytable2 <- mytable
        mytable[mytable2]  <- "x"
        mytable[!mytable2] <- "-"
    }
    
    if(is.null(colnames(mytable))) colnames(mytable) <- rep(" ", ncol(mytable))
    
    nchars <- nchar(colnames(mytable))
    colnames(mytable)[nchars == 1] <- format(colnames(mytable)[nchars == 1], width = 2, justify = "centre")
    nchars[nchars == 1] <- 2
    
    for (i in seq((ncol(mytable) - any(colnames(mytable) == "lines")))) {
        mytable[, i] <- format(format(mytable[, i]), width = nchars[i], justify = "centre")
    }
    
    rownames(mytable) <- paste(rownames(mytable), "")
    
    return(noquote(mytable))
}

