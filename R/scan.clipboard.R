scan.clipboard <- function (...) {
    dots <- list(...)
    
    if (Sys.info()[['sysname']] == "Darwin") {
        clipboard <- readLines(textConnection(system("pbpaste", intern = TRUE)))
        sep <- ifelse(is.null(dots$sep), "\t", dots$sep)
        clipboard <- unlist(strsplit(clipboard, split = sep))
    } else if (Sys.info()[['sysname']] == "Windows") {
        dots$file <- "clipboard"
        clipboard <- do.call("scan", dots)
    }
        
    clipboard <- clipboard[clipboard != ""]
    
    if (possibleNumeric(clipboard)) {
        return(asNumeric(clipboard))
    } else {
        return(clipboard)
    }
}
