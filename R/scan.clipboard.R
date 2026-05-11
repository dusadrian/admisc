#' Cross platform scan/write clipboard
#'
#' Functions to read and write to the system's clipboard, for copy/paste operations.
#'
#' @name scan.clipboard
#' @rdname clipboard
#' @aliases write.clipboard
#' @rawRd
#' \usage{
#' scan.clipboard(...)
#' write.clipboard(x)
#' }
#'
#' \arguments{
#'     \item{x}{Object to be written to the clipboard}
#'     \item{...}{Same arguments that are used in the base function \bold{\code{scan}}}
#' }
#'
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \keyword{functions}
NULL
#' @export
scan.clipboard <- function (...) {
    dots <- list(...)
    
    if (Sys.info()[['sysname']] == "Darwin") {
        clipboard <- readLines(textConnection(system("pbpaste", intern = TRUE)))
        sep <- ifelse(is.null(dots$sep), "\t", dots$sep)
        clipboard <- unlist(strsplit(clipboard, split = sep))
    } else if (Sys.info()[['sysname']] == "Windows") {
        if (is.null(dots$sep)) {
            dots$sep <- "\t"
        }
        if (is.null(dots$what)) {
            dots$what <- character()
        }
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
