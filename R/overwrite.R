#' Overwrite an object in a given environment.
#'
#' Utility function to overwrite an object, and bypass the assignment operator.
#'
#' @name overwrite
#' @rdname overwrite
#' @rawRd
#' \usage{
#' overwrite(objname, content, environment)
#' }
#'
#' \arguments{
#'     \item{objname}{Character, the name of the object to overwrite.}
#'     \item{content}{An R object}
#'     \item{environment}{The environment where to perform the overwrite procedure.}
#' }
#'
#' \details{
#' \code{assign()} is sufficient when \code{objname} is a simple object name,
#' such as \code{"bar"}. It is not sufficient when the target is an expression,
#' such as \code{"bar$A"}. A call such as \code{assign(bar$A, 1, envir =
#' parent.frame())} fails because \code{assign()} expects its first argument to
#' evaluate to a character string. If that expression is first deparsed, for
#' instance to \code{"bar$A"}, then \code{assign()} would create an object
#' literally named \code{"bar$A"} in the target environment rather than
#' replacing component \code{A} inside \code{bar}.
#'
#' This function handles both situations. For simple names, it overwrites the
#' object directly in the target environment. For expressions, it reconstructs
#' and evaluates the corresponding assignment call in that environment.
#' }
#'
#' \value{
#' This function does not return anything.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' foo <- function(object, x) {
#'     objname <- deparse(substitute(object))
#'     overwrite(objname, x, parent.frame())
#' }
#'
#'
#' bar <- 1
#' foo(bar, 2)
#'
#' bar
#' # [1] 2
#'
#' bar <- list(A = bar)
#' foo(bar$A, 3)
#'
#' bar
#' # $A
#' # [1] 3
#'
#'
#' foo_assign <- function(object, x) {
#'     objname <- deparse(substitute(object))
#'     assign(objname, x, envir = parent.frame())
#' }
#'
#' bar <- list(A = 1)
#' try(assign(bar$A, 3, envir = parent.frame()))
#'
#' bar <- 1
#' foo_assign(bar, 2)
#'
#' bar
#' # [1] 2
#'
#' bar <- list(A = 1)
#' foo_assign(bar$A, 3)
#'
#' bar
#' # $A
#' # [1] 1
#'
#' `bar$A`
#' # [1] 3
#' }
#'
#' \keyword{functions}
NULL
#' @export
`overwrite` <- function(objname, content, environment) {
    objname <- gsub("'|\"|[[:space:]]", "", objname)
    if (exists(objname, environment)) {
        environment[[objname]] <- content
    }
    else {
        structure_string <- paste(capture.output(dput(content)), collapse = " ")

        eval(
            parse(text = sprintf(paste(objname, "<- %s"), structure_string)),
            envir = environment
        )
    }
}
