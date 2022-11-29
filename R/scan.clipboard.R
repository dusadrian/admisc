scan.clipboard <- function (...) {
    dots <- list(...)
    
    if (Sys.info()[['sysname']] == "Darwin") {
        clipboard <- readLines(textConnection(system("pbpaste", intern = TRUE)))
        clipboard <- clipboard[clipboard != ""]
        
        if (!is.null(dots$sep)) {
            clipboard <- unlist(strsplit(clipboard, split = dots$sep))
        }
    } else if (Sys.info()[['sysname']] == "Windows") {
        clipboard <- do.call("scan", dots)
    }
        
    if (possibleNumeric(clipboard)) {
        return(asNumeric(clipboard))
    } else {
        return(clipboard)
    }
}
