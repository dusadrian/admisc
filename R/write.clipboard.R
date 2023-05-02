write.clipboard <- function (x) {
    ### TODO: special treatment for the w_table() objects
    if (Sys.info()[['sysname']] == "Darwin") {
        clipboard <- pipe("pbcopy", "w")
        write.table(x, file = clipboard)
        close(clipboard)
    } else if (Sys.info()[['sysname']] == "Windows") {
        write.table(x, "clipboard", sep = "\t") 
    }
}
