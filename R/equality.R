#' Check difference and / or (in)equality of numbers
#'
#' Check if one number is greater / lower than (or equal to) another.
#'
#' @name agtb
#' @rdname equality
#' @aliases altb
#' @aliases agteb
#' @aliases alteb
#' @aliases aeqb
#' @aliases aneqb
#' @rawRd
#' \usage{
#' agtb(a, b, bincat)
#' altb(a, b, bincat)
#' agteb(a, b, bincat)
#' alteb(a, b, bincat)
#' aeqb(a, b, bincat)
#' aneqb(a, b, bincat)
#' }
#'
#' \arguments{
#'   \item{a}{Numerical vector}
#'   \item{b}{Numerical vector}
#'   \item{bincat}{Binary categorization values, an atomic vector of length 2}
#' } 
#'
#' \details{
#' Not all numbers (especially the decimal ones) can be represented exactly in
#' floating point arithmetic, and their arithmetic may not give the normal expected
#' result.
#'
#' This set of functions check for the in(equality) between two numerical vectors a
#' and b, with the following name convention:
#'
#' \bold{\code{gt}} means \dQuote{greater than}
#'
#' \bold{\code{lt}} means a \dQuote{lower than} b
#'
#' \bold{\code{gte}} means a \dQuote{greater than or equal to} b
#'
#' \bold{\code{lte}} means a \dQuote{lower than or equal to} b
#'
#' \bold{\code{eq}} means a \dQuote{equal to} b
#'
#' \bold{\code{neq}} means a \dQuote{not equal to} b
#'
#' The argument \bold{\code{values}} is useful to replace the TRUE / FALSE values
#' with custom categories.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#'
#' \references{
#' Goldberg, David (1991) "What Every Computer Scientist Should Know About
#' Floating-point Arithmetic", ACM Computing Surveys vol.23, no.1, pp.5-48,
#' \doi{10.1145/103162.103163}
#' }
#'
#'
#' \keyword{functions}
NULL
`undeclareit` <- function(x, drop = FALSE, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    
    # this is necessary to replace those values
    # (because of the "[<-.declared" method)
    attributes(x) <- NULL # or x <- unclass(x), but I find this cleaner
    if (!is.null(na_index)) {
        # x <- ifelse(!is.na(missingValues), missingValues, x)
        x[na_index] <- names(na_index)
    }
    
    x <- coerceMode(x)
    
    attrx$na_index <- NULL
    attrx$na_values <- NULL
    attrx$na_range <- NULL

    if (isFALSE(drop)) {
        attributes (x) <- attrx
    }

    return(x)
}

#' @export
`agtb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- (a - tol) > b
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

#' @export
`altb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- a < (b - tol)
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

#' @export
`agteb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- (a + tol) > b
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

#' @export
`alteb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- a < (b + tol)
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

#' @export
`aeqb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- abs(a - b) < tol
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}

#' @export
`aneqb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- abs(a - b) > tol
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
