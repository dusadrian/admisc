#' @export
#' @noRd
`prettyString` <-
function(string.vector, string.width = 80, repeat.space = 5, separator = ",", sufnec = "",
         outcome = "", cases = FALSE) {
    
    if (length(string.vector) == 1) {
        if (nchar(encodeString(paste(string.vector, " ", sufnec, " ", outcome, sep=""))) >= string.width) {
            string.vector <- unlist(strsplit(string.vector, split = paste(" \\", separator, " ", sep = ""), useBytes = TRUE))
        }
    }
    
    if (length(string.vector) <= 1L) {
        if (outcome == "") return(string.vector[1L])
        return(paste(string.vector[1L], sufnec, outcome))
    }
    prefix <- if (separator == ";") "" else " "
    delim <- paste0(prefix, separator, " ")
    indent <- strrep(" ", repeat.space)
    pieces <- string.vector
    start <- 1L
    for (j in 2:length(pieces)) {
        end <- if (separator == ";") j - 1L else j
        line <- paste(string.vector[start:end], collapse = delim)
        if (nchar(encodeString(line)) >= string.width) {
            pieces[j] <- paste0(prefix, separator, "\n", indent, string.vector[j])
            start <- j
        } else {
            pieces[j] <- paste0(delim, string.vector[j])
        }
    }
    suffix <- ""
    if (outcome != "") {
        line <- paste(string.vector[start:length(pieces)], collapse = delim)
        wrap <- nchar(encodeString(paste(line, sufnec, outcome))) >= string.width
        suffix <- paste0(if (wrap) paste0("\n", indent) else " ", sufnec, " ", outcome)
    }
    paste0(paste0(pieces, collapse = ""), suffix)
}
