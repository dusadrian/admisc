#' Extract information from a multi-value SOP/DNF expression
#'
#' Functions to extract information from an expression written in SOP, or in the
#' canonical DNF, for multi-value causal conditions. They extract either the
#' values within brackets, or the causal condition names outside the brackets.
#'
#' @name betweenBrackets
#' @rdname brackets
#' @aliases insideBrackets
#' @aliases outsideBrackets
#' @aliases curlyBrackets
#' @aliases squareBrackets
#' @aliases roundBrackets
#' @usage
#' betweenBrackets(x, type = "[", invert = FALSE, regexp = NULL)
#' outsideBrackets(x, type = "[", regexp = NULL)
#' curlyBrackets(x, outside = FALSE, regexp = NULL)
#' squareBrackets(x, outside = FALSE, regexp = NULL)
#' roundBrackets(x, outside = FALSE, regexp = NULL)
#'
#' @param x A DNF/SOP expression.
#' @param type Brackets type: curly, round or square.
#' @param invert Logical, if activated returns whatever is not within the
#' brackets.
#' @param outside Logical, if activated returns the condition names outside the
#' brackets.
#' @param regexp Optional regular expression to extract information with.
#' @param expression A DNF/SOP expression.
#' @param snames A string containing the sets' names, separated by commas.
#' @param noflevels Numerical vector containing the number of levels for each
#' set.
#' @param simplify Logical, remove redundant expressions after expansion.
#'
#' @details
#' Expressions written in SOP are used in Boolean logic, signaling a
#' disjunction of conjunctions.
#'
#' These expressions are useful in Qualitative Comparative Analysis, a social
#' science methodology used to search for causal configurations associated with
#' a certain outcome.
#'
#' They are also used to draw Venn diagrams with package `venn`, which draws
#' any kind of set intersection based on a custom SOP expression.
#'
#' `curlyBrackets()`, `squareBrackets()` and `roundBrackets()` are special cases
#' of `betweenBrackets()` and `outsideBrackets()`, using curly, square or round
#' brackets through the `type` argument.
#'
#' `outsideBrackets()` can also be seen as a special case of
#' `betweenBrackets(invert = TRUE)`.
#'
#' SOP expressions are usually written using curly brackets for multi-value
#' conditions but, to allow evaluation of unquoted expressions through R's
#' parser, unquoted expressions should use square brackets and conjunctions
#' should always use the product `*` sign.
#'
#' Sufficiency is recognized as `"=>"` in quoted expressions but this does not
#' pass over R's parsing system in unquoted expressions. To overcome this
#' problem, it is best to use the single arrow `"->"` notation. Necessity is
#' recognized as either `"<="` or `"<-"`, both being valid in quoted and
#' unquoted expressions.
#'
#' @author Adrian Dusa
#'
#' @examples
#' sop <- "A[1] + B[2]*C[0]"
#'
#' betweenBrackets(sop)
#' betweenBrackets(sop, invert = TRUE)
#'
#' # unquoted (valid) SOP expressions are allowed, same result
#' betweenBrackets(A[1] + B[2]*C[0])
#'
#' # curly brackets are also valid in quoted expressions
#' betweenBrackets("A{1} + B{2}*C{0}", type = "{")
#' curlyBrackets("A{1} + B{2}*C{0}")
#' curlyBrackets("A{1} + B{2}*C{0}", outside = TRUE)
#'
#' squareBrackets(A[1] + B[2]*C[0])
#' squareBrackets(A[1] + B[2]*C[0], outside = TRUE)
#'
#' @keywords functions
NULL
#' @export
`betweenBrackets` <- function(x, type = "[", invert = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)

    tml <- which(typematrix == type, arr.ind = TRUE)[1]

    if (is.na(tml)) {
        tml <- 1
    }

    tml <- typematrix[tml, 1:2]
    if (is.null(regexp)) {
        regexp <- "[[:alnum:]|,]*"
    }

    result <- gsub(
        paste("\\", tml, sep = "", collapse = "|"),
        "",
        regmatches(
            x,
            gregexpr(
                paste("\\", tml, sep = "", collapse = regexp),
                x
            ),
            invert = invert
        )[[1]]
    )
    # return(trimstr(result[result != ""]))
    result <- gsub("\\*|\\+", "", unlist(strsplit(gsub("\\s+", " ", result), split = " ")))
    return(result[result != ""])
}


#' @export
`insideBrackets` <- function(...) {
    .Deprecated(msg = "Function insideBrackets() is deprecated, use betweenBrackets().\n")
    betweenBrackets(...)
}



#' @export
`outsideBrackets` <- function(x, type = "[", regexp = NULL) {
    x <- recreate(substitute(x))
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    tml <- which(typematrix == type, arr.ind = TRUE)[1]
    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    if (is.null(regexp)) {
        regexp <- "[[:alnum:]|,]*"
    }
    pattern <- paste("\\", tml, sep = "", collapse = regexp)
    result <- gsub(
        "\\*|\\+",
        "",
        unlist(
            strsplit(
                gsub(
                    "\\s+",
                    " ",
                    trimstr(gsub(pattern, " ", x))
                ),
                split = " "
            )
        )
    )
    return(result[result != ""])
}


#' @export
`curlyBrackets` <- function(x, outside = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    # just in case it was previously split
    x <- paste(x, collapse = "+")

    if (is.null(regexp)) {
        regexp <- "\\{[[:alnum:]|,|;]+\\}"
    }

    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- gsub(
            "\\*",
            "",
            unlist(strsplit(res, split = "\\+"))
        )
        return(res[res != ""])
    }
    else {
        return(gsub("\\{|\\}|\\*", "", res))
    }
}


#' @export
`squareBrackets` <- function(x, outside = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    # just in case it was previously split
    x <- paste(x, collapse = "+")
    if (is.null(regexp)) {
        regexp <- "\\[[[:alnum:]|,|;]+\\]"
    }
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- gsub(
            "\\*",
            "",
            unlist(strsplit(res, split = "\\+"))
        )
        return(res[res != ""])
    }
    else {
        return(gsub("\\[|\\]|\\*", "", res))
    }
}


#' @export
`roundBrackets` <- function(x, outside = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    if (is.null(regexp)) {
        regexp <- "\\(([^)]+)\\)"
    }
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- unlist(strsplit(res, split="\\+"))
        return(res[res != ""])
    }
    else {
        return(gsub("\\(|\\)|\\*", "", res))
    }
}


#' @export
`expandBrackets` <- function(
    expression, snames = "", noflevels = NULL, scollapse = FALSE
) {

    expression <- recreate(substitute(expression))
    snames <- splitstr(snames)
    star <- any(grepl("[*]", expression))
    multivalue <- any(grepl("\\[|\\]|\\{|\\}", expression))
    collapse <- ifelse(
        any(nchar(snames) > 1) | multivalue | star | scollapse,
        "*",
        ""
    )

    curly <- grepl("[{]", expression)
    sl <- ifelse( # single letters
        identical(snames, ""),
        FALSE,
        ifelse(
            all(nchar(snames) == 1),
            TRUE,
            FALSE
        )
    )

    getbl <- function(expression, snames = "", noflevels = NULL) {
        bl <- splitMainComponents(gsub("[[:space:]]", "", expression))
        bl <- splitBrackets(bl)
        # detect something like ~(A + B)
        bl <- lapply(bl, function(x) {
            if (tilde1st(x[[1]]) & nchar(x[[1]]) == 1) {
                x <- x[-1]
                x[[1]] <- as.character(negate(x[[1]], snames = snames, noflevels = noflevels))
            }
            return(x)
        })
        bl <- removeSingleStars(bl)
        bl <- splitPluses(bl)
        blu <- unlist(bl)
        # to detect something like AC + B~C with no snames (it has a tilde, but not first)
        bl <- splitStars(
            bl,
            ifelse(
                (
                    sl | any(
                        hastilde(blu) & !tilde1st(blu)
                    )
                ) & !grepl("[*]", expression) & !multivalue,
                "",
                "*"
            )
        )
        bl <- solveBrackets(bl)
        bl <- simplifyList(bl)
        return(bl)
    }


    bl <- getbl(expression, snames = snames, noflevels = noflevels)
    if (length(bl) == 0) return("")

    bl <- paste(
        unlist(
            lapply(
                bl,
                paste,
                collapse = collapse
            )
        ),
        collapse = " + "
    )

    expressions <- translate(bl, snames = snames, noflevels = noflevels)
    snames <- colnames(expressions)

    redundant <- logical(nrow(expressions))

    if (nrow(expressions) > 1) {
        for (i in seq(nrow(expressions) - 1)) {
            if (!redundant[i]) {
                for (j in seq(i + 1, nrow(expressions))) {
                    if (!redundant[j]) {
                        subsetrow <- checkSubset(
                            expressions[c(i, j), , drop = FALSE],
                            implicants = FALSE
                        )

                        if (!is.null(subsetrow)) {
                            redundant[c(i, j)[subsetrow]] <- TRUE
                        }
                    }
                }
            }
        }

        expressions <- expressions[!redundant, , drop = FALSE]
        if (possibleNumeric(expressions)) {

            mat <- matrix(asNumeric(expressions) + 1, nrow = nrow(expressions))
            colnames(mat) <- colnames(expressions)
            expressions <- sortExpressions(mat) - 1

        }
        else {
            eorder <- order(
                apply(
                    expressions,
                    1,
                    function(x) sum(x < 0)
                ),
                decreasing = TRUE
            )

            expressions <- expressions[eorder, , drop = FALSE]
        }
    }



    # this is different from writePIs() because the entry matrix
    # is not necessarily numeric, it can contain "1,2" for instance
    expressions <- unlist(apply(expressions, 1, function(x) {
        result <- c()
        for (i in seq(length(snames))) {
            if (x[i] != -1) {
                if (multivalue) {
                    result <- c(
                        result,
                        paste(
                            snames[i],
                            ifelse(curly, "{", "["),
                            x[i],
                            ifelse(curly, "}", "]"),
                            sep = ""
                        )
                    )
                }
                else {
                    if (x[i] == 0) {
                        result <- c(result, paste("~", snames[i], sep = ""))
                    }
                    else {
                        result <- c(result, snames[i])
                    }
                }
            }
        }
        return(paste(result, collapse = collapse))
    }))


    return(paste(expressions, collapse = " + "))
}
