`prettyTable` <-
function(input) {
    
    if (methods::is(input, "QCA_pic")) {
        class(input) <- "matrix" # to remove the class "pic"
    }
    else {
        input <- as.matrix(input) # to make sure it's a matrix
    }
    
    if (is.logical(input)) {
        input2 <- input
        input[input2]  <- "x"
        input[!input2] <- "-"
    }
    
    if(is.null(colnames(input))) colnames(input) <- rep(" ", ncol(input))
    
    nchars <- nchar(colnames(input))
    colnames(input)[nchars == 1] <- format(colnames(input)[nchars == 1], width = 2, justify = "centre")
    nchars[nchars == 1] <- 2
    
    for (i in seq((ncol(input) - any(colnames(input) == "lines")))) {
        input[, i] <- format(format(input[, i]), width = nchars[i], justify = "centre")
    }
    
    rownames(input) <- paste(rownames(input), "")
    
    return(noquote(input))
}

