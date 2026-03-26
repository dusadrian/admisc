#' Tilde operations
#'
#' Checks and changes expressions containing set negations using a tilde.
#'
#' @name hastilde
#' @rdname tilde
#' @aliases notilde
#' @aliases tilde1st
#' @rawRd
#' \usage{
#' hastilde(x)
#' notilde(x)
#' tilde1st(x)
#' }
#'
#' \arguments{
#'   \item{x}{A vector of values}
#' }
#'
#'
#' \details{
#' Boolean expressions can be negated in various ways. For binary crisp and fuzzy sets, one of
#' the most straightforward ways to invert the set membership scores is to subtract them from 1.
#' This is both possible using R vectors and also often used to signal a negation in SOP
#' (sum of products) expressions. 
#'
#' Some other times, SOP expressions can signal a set negation (also known as the absence of a 
#' causal condition) by using lower case letters, while upper case letters are used to signal
#' the presence of a causal condition. SOP expressions also use a tilde to signal a set negation,
#' immediately preceding the set name.
#'
#' This set of functions detect when and if a set present in a SOP expression contains a tilde
#' (function \bold{\code{hastilde}}), whether the entire expression begins with a tilde (function
#' \bold{\code{tilde1st}}).
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' hastilde("~A")
#' }
#'
#'
#' \keyword{functions}
NULL
#' @export
`tilde1st` <- function(x) {
    is.element(
        substring(
            gsub(
                paste0("[[:space:]|", "\u00a0", "]"), # multibyte space
                "",
                x
            ),
            1, 1
        ),
        tildae()
    )
}

#' @export
`hastilde` <- function(x) {
    grepl(paste(tildae(), collapse = "|"), x)
}

#' @export
`notilde` <- function(x) {
    gsub(
        paste(tildae(), collapse = "|"),
        "",
        gsub(
            paste0("[[:space:]|", "\u00a0", "]"), # multibyte space
            "",
            x
        )
    )
}
