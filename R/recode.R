#' Recode a variable
#'
#' Recodes a vector (numeric, character or factor) according to a set of rules.
#' It is similar to the function \bold{\code{recode}()} from package \pkg{car},
#' but more flexible. It also has similarities with the function
#' \bold{\code{\link[base]{findInterval}()}} from package \bold{\pkg{base}}.
#'
#' @name recode
#' @rdname recode
#' @rawRd
#' \usage{
#' recode(x, rules = NULL, cut = NULL, values = NULL, ...)
#' }
#'
#' \arguments{
#'     \item{x}{A vector of mode numeric, character or factor.}
#'     \item{rules}{Character string or a vector of character strings
#'                  for recoding specifications.}
#'     \item{cut}{A vector of one or more unique cut points.}
#'     \item{values}{A vector of output values.}
#'     \item{...}{Other parameters, for compatibility with other functions such as
#'                \bold{\code{recode}()} in package \pkg{car} but also
#'          \bold{\code{\link[base]{factor}()}} in package \bold{\pkg{base}}}
#' }
#'
#' \details{
#' Similar to the \bold{\code{recode()}} function in package \pkg{car}, the
#' recoding rules are separated by semicolons, of the form \bold{\code{input = output}},
#' and allow for:
#'
#'
#' \tabular{rl}{
#'     a single value \tab \bold{\code{1 = 0}}\cr
#'     a range of values \tab \bold{\code{2:5 = 1}}\cr
#'     a set of values \tab \bold{\code{c(6,7,10) = 2}}\cr
#'     \bold{\code{else}} \tab everything that is not covered by the previously specified rules
#' }
#'
#' Contrary to the \bold{\code{recode}()} function in package \pkg{car}, this
#' function allows the \bold{\code{:}} sequence operator (even for factors), so
#' that a rule such as \bold{\code{c(1,3,5:7)}}, or \bold{\code{c(a,d,f:h)}} would
#' be valid.
#'
#' Actually, since all rules are specified in a string, it really doesn't matter
#' if the \bold{\code{c()}} function is used or not. For compatibility reasons it
#' accepts it, but a more simple way to specify a set of rules is
#' \bold{\code{"1,3,5:7=A; else=B"}}
#'
#' Special values \bold{\code{lo}} and \bold{\code{hi}} may also appear in the
#' range of values, while \bold{\code{else}} can be used with \bold{\code{else=copy}}
#' to copy all values which were not specified in the recoding rules.
#'
#' In the package \pkg{car}, a character \bold{\code{output}} would have to be quoted,
#' like \bold{\code{"1:2='A'"}} but that is not mandatory in this function, \bold{\code{"1:2=A"}}
#' would do just as well. Output values such as \bold{\code{"NA"}} or \bold{\code{"missing"}}
#' are converted to \bold{\code{NA}}.
#'
#' Another difference from the \pkg{car} package: the output is \bold{not} automatically
#' converted to a factor even if the original variable is a factor. That option is left to the
#' user's decision to specify \bold{\code{as.factor.result}}, defaulted to \bold{\code{FALSE}}.
#'
#' A capital difference is the treatment of the values not present in the recoding rules. By
#' default, package \pkg{car} copies all those values in the new object, whereas in this
#' package the default values are \bold{\code{NA}} and new values are added only if they are
#' found in the rules. Users can choose to copy all other values not present in the recoding
#' rules, by specifically adding \bold{\code{else=copy}} in the rules.
#'
#' Since the two functions have the same name, it is possible that users loading both
#' packages to use one instead of the other (depending which package is loaded first).
#' In order to preserve functionality and minimize possible namespace collisions with package
#' \pkg{car}, special efforts have been invested to ensure perfect compatibility with
#' the other \bold{\code{recode}()} function (plus more).
#'
#' The argument \bold{\code{...}} allows for more arguments specific to the \pkg{car} package,
#' such as \bold{\code{as.factor.result}}, \bold{\code{as.numeric.result}}. In addition, it also
#' accepts \bold{\code{levels}}, \bold{\code{labels}} and \bold{\code{ordered}} specific to function
#' \bold{\code{\link[base]{factor}()}} in package \bold{\pkg{base}}. When using the arguments
#' \bold{\code{levels}} and / or \bold{\code{labels}}, the output will automatically be coerced
#' to a factor, unless the argument \bold{\code{values}} is used, as indicated below.
#'
#' Blank spaces outside category labels are ignored, see the last example.
#'
#' It is possible to use \bold{\code{recode()}} in a similar way to function
#' \bold{\code{cut()}}, by specifying a vector of cut points. For any number of
#' such \bold{\code{c}} cut ploints, there should be \bold{\code{c + 1}} values.
#' If not otherwise specified, the argument \bold{\code{values}} is automatically
#' constructed as a sequence of numbers from \bold{\code{1}} to \bold{\code{c + 1}}.
#'
#' Unlike the function \bold{\code{cut()}}, arguments such as
#' \bold{\code{include.lowest}} or \bold{\code{right}} are not necessary because
#' the final outcome can be changed by tweaking the cut values.
#'
#' If both arguments \bold{\code{values}} and \bold{\code{labels}} are provided,
#' the labels are going to be stored as an attribute.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' x <- rep(1:3, 3)
#' #  [1] 1 2 3 1 2 3 1 2 3
#'
#' recode(x, "1:2 = A; else = B")
#' #  [1] "A" "A" "B" "A" "A" "B" "A" "A" "B"
#'
#' recode(x, "1:2 = 0; else = copy")
#' #  [1] 0 0 3 0 0 3 0 0 3
#'
#'
#' set.seed(1234)
#' x <- sample(18:90, 20, replace = TRUE)
#' #  [1] 45 39 26 22 55 33 21 87 31 73 79 21 21 38 57 73 84 22 83 64
#'
#' recode(x, cut = "35, 55")
#' #  [1] 2 2 1 1 2 1 1 3 1 3 3 1 1 2 3 3 3 1 3 3
#'
#' set.seed(1234)
#' x <- factor(sample(letters[1:10], 20, replace = TRUE),
#'           levels = letters[1:10])
#' #  [1] j f e i e f d b g f j f d h d d e h d h
#' # Levels: a b c d e f g h i j
#'
#' recode(x, "b:d = 1; g:hi = 2; else = NA") # note the "hi" special value
#' #  [1]  2 NA NA  2 NA NA  1  1  2 NA  2 NA  1  2  1  1 NA  2  1  2
#'
#' recode(x, "a, c:f = A; g:hi = B; else = C", labels = "A, B, C")
#' #  [1] B A A B A A A C B A B A A B A A A B A B
#' # Levels: A B C
#'
#' recode(x, "a, c:f = 1; g:hi = 2; else = 3",
#'        labels = c("one", "two", "three"), ordered = TRUE)
#' #  [1] two   one   one   two   one   one   one   three two   one
#' # [11] two   one   one   two   one   one   one   two   one   two
#' # Levels: one < two < three
#'
#' set.seed(1234)
#' categories <- c("An", "example", "that has", "spaces")
#' x <- factor(sample(categories, 20, replace = TRUE),
#'             levels = categories, ordered = TRUE)
#' sort(x)
#' #  [1] An       An       An       example  example  example  example
#' #  [8] example  example  example  example  that has that has that has
#' # [15] spaces   spaces   spaces   spaces   spaces   spaces
#' # Levels: An < example < that has < spaces
#'
#' recode(sort(x), "An : that has = 1; spaces = 2")
#' #  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
#'
#' # single quotes work, but are not necessary
#' recode(sort(x), "An : 'that has' = 1; spaces = 2")
#' #  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
#'
#' # same using cut values
#' recode(sort(x), cut = "that has")
#' #  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
#'
#' # modifying the output values
#' recode(sort(x), cut = "that has", values = 0:1)
#' #  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1
#'
#' # more treatment of "else" values
#' x <- 10:20
#'
#' # recoding rules don't overlap all existing values, the rest are empty
#' recode(x, "8:15 = 1")
#' #  [1]  1  1  1  1  1  1 NA NA NA NA NA
#'
#' # all other values copied
#' recode(x, "8:15 = 1; else = copy")
#' #  [1]  1  1  1  1  1  1 16 17 18 19 20
#'
#' }
#'
#' \keyword{functions}
NULL
#' @export
`recode` <- function(x, rules = NULL, cut = NULL, values = NULL, ...) {
    UseMethod("recode")
}


#' @export
`recode.declared` <- function(x, rules = NULL, cut = NULL, values = NULL, ...) {

    dots <- list(...)
    na_index <- attr(x, "na_index")
    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")
    xlabels <- attr(x, "labels", exact = TRUE)
    attributes(x) <- NULL

    labels <- splitstr(dots[["labels"]])
    label <- dots[["label"]]
    x <- recode(x = x, rules = rules, cut = cut, values = values)

    if (is.null(names(labels))) {
        values <- sort(unique(x))
        if (!is.null(labels)) {
            if (length(values) == length(labels)) {
                names(values) <- labels
                labels <- values
            } else {
                stopError("The number of labels should be equal to the number of recodings.")
            }
        }
    }

    if (is.null(na_index)) { # because of drop_na() for instance
        xlabels <- NULL
    } else {
        attr(x, "na_index") <- na_index
        attr(x, "na_values") <- na_values
        attr(x, "na_range") <- na_range
    }

    if (!is.null(xlabels)) {
        if (!is.null(na_values)) {
            xlabels <- xlabels[is.element(xlabels, na_values)]
        }
        else if (!is.null(na_range)) {
            xlabels <- xlabels[xlabels >= na_range[1] & xlabels <= na_range[2]]
        }
    }

    attr(x, "labels") <- c(labels, xlabels)
    attr(x, "label") <- label
    class(x) <- c("declared", class(x))

    return(x)
}


#' @export
`recode.default` <- function(x, rules = NULL, cut = NULL, values = NULL, ...) {

    if (missing(x)) {
        stopError("Argument 'x' is missing.")
    }

    if (!is.atomic(x))   {
        stopError("The input 'x' should be an atomic vector / factor.")
    }

    if (all(is.na(x))) {
        stopError("Nothing to recode, all values are missing.")
    }

    dots <- recreate(list(...))
    as.factor.result  <- isTRUE(dots$as.factor.result)
    as.numeric.result <- !isFALSE(dots$as.numeric.result)
    factor.levels     <- splitstr(dots$levels)
    factor.labels     <- splitstr(dots[["labels"]])
    factor.ordered    <- FALSE

    if (is.element("ordered", names(dots))) {
        factor.ordered <- dots$ordered
    }
    else if (is.element("ordered_result", names(dots))) {
        factor.ordered <- dots$ordered_result
    }

    if (is.element("cuts", names(dots)) & missing(cut)) {
        # backwards compatibility
        cut <- dots[["cuts"]]
    }

    if (is.logical(factor.labels)) { # something like: factor.labels = TRUE
        factor.labels <- character(0)
    }

    if (!is.null(factor.levels) || !is.null(factor.labels)) {
        as.factor.result  <- TRUE
    }

    `getFromRange` <- function(a, b, uniques, xisnumeric) {
        copya <- a
        copyb <- b

        a <- ifelse(a == "lo", uniques[1], a)
        b <- ifelse(b == "hi", uniques[length(uniques)], b)

        if (xisnumeric) {
            a <- asNumeric(a)
            b <- asNumeric(b)
            if (a > b & (copya == "lo" | copyb == "hi")) return(NULL)
        }

        seqfrom <- which(uniques == a)
        seqto <- which(uniques == b)

        temp2 <- sort(unique(c(uniques, a, b)))

        if (length(seqfrom) == 0) {
            seqfrom <- which(uniques == temp2[which(temp2 == a) + 1])
        }

        if (length(seqto) == 0) {
            seqto <- which(uniques == temp2[which(temp2 == b) - 1])
        }

        if (length(c(seqfrom, seqto)) < 2) return(NULL)

        return(seq(seqfrom, seqto))
    }

    if (is.null(cut)) {
        if (is.null(rules)) {
            stopError("At least one argument 'rules' or 'cut' should be provided.")
        }

        rules <- gsub(
            "\n|\t", "", gsub(
                "'", "", gsub(
                    ")", "", gsub(
                        "c(", "", rules, fixed = TRUE
                    )
                )
            )
        )

        if (length(rules) == 1) {
            semicolons <- gsub("[^;]", "", rules)
            equals <- gsub("[^=]", "", rules)

            if (nchar(equals) != nchar(semicolons) + 1) {
                stopError("The rules should be separated by a semicolon.")
            }

            rules <- unlist(strsplit(rules, split = ";"))
        }

        rulsplit <- strsplit(rules, split = "=")

        oldval <- trimws(sapply(rulsplit, "[", 1))
        newval <- trimws(sapply(rulsplit, "[", 2))

        if (!is.null(factor.labels)) {
            if (length(factor.labels) != length(newval)) {
                stopError("The number of labels should be equal to the number of recodings.")
            }
        }

        temp <- rep(NA, length(x))

        elsecopy <- oldval == "else" & newval == "copy"

        if (any(elsecopy)) {
            if (is.factor(x)) {
                temp <- as.character(x)
            }
            else {
                temp <- x
            }

            newval <- newval[!elsecopy]
            oldval <- oldval[!elsecopy]
        }

        newval[newval == "missing" | newval == "NA"] <- NA

        if (any(oldval == "else")) {
            if (sum(oldval == "else") > 1) {
                stopError("Too many \"else\" statements.")
            }

            # place the "else" statement as the last one, very important
            whichelse <- which(oldval == "else")
            oldval <- c(oldval[-whichelse], oldval[whichelse])
            newval <- c(newval[-whichelse], newval[whichelse])
        }

        oldval <- lapply(
            lapply(
                lapply(oldval, strsplit, split = ","),
                "[[", 1
            ),
            function(y) {
                lapply(
                    strsplit(y, split = ":"),
                    trimstr
                )
            }
        )

        newval <- trimstr(rep(newval, unlist(lapply(oldval, length))))

        # for (i in seq(length(newval))) {
        #     tc <- tryCatch(eval(parse(text = newval[[i]])), error = function(e) e)
        #     if (!(is.list(tc) && identical(names(tc), c("message", "call")))) {
        #         newval[i] <- tc
        #     }
        # }
        #
        # if (possibleNumeric(newval)) {
        #     newval <- asNumeric(newval)
        # }

        if (any(unlist(lapply(oldval, function(y) lapply(y, length))) > 2)) {
            stopError("Too many : sequence operators.")
        }


        from <- unlist(lapply(oldval, function(y) lapply(y, "[", 1)))
        to <- unlist(lapply(oldval, function(y) lapply(y, "[", 2)))

        uniques <- if(is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))

        recoded <- NULL
        xisnumeric <- possibleNumeric(uniques)

        if (xisnumeric) {
            x <- asNumeric(x) # to be safe
            uniques <- asNumeric(uniques)
        }


        for (i in seq(length(from))) {
            if (!is.na(to[i])) { # a range
                torecode <- getFromRange(from[i], to[i], uniques, xisnumeric)
                if (!is.null(torecode)) {
                    vals <- uniques[torecode]
                    temp[is.element(x, vals)] <- newval[i]
                    recoded <- c(recoded, vals)
                }

            }
            else { # a single value

                # "else" should (must?) be the last rule
                if (from[i] == "else") {
                    temp[!is.element(x, recoded)] <- newval[i]
                }
                else if (from[i] == "missing" | from[i] == "NA") {
                    temp[is.na(x)] <- newval[i]
                }
                else {
                    # if (!any(x == from[i])) {
                    #     val <- ifelse(is.na(suppressWarnings(as.numeric(from[i]))), paste("\"", from[i], "\"", sep = ""), from[i])
                    #     stopError(paste0("The value", val, "was not found."))
                    # }
                    temp[x == from[i]] <- newval[i]
                }

                recoded <- c(recoded, from[i])
            }
        }
    }
    else {

        if (length(cut) == 1 & is.character(cut)) {
            cut <- gsub(
                "\n|\t", "", gsub(
                    "'", "", gsub(
                        ")", "", gsub(
                            "c(", "", cut, fixed = TRUE
                        )
                    )
                )
            )
            cut <- trimstr(unlist(strsplit(cut, split = ",")))
            if (length(cut) == 1) {
                cut <- trimstr(unlist(strsplit(cut, split = ";")))
            }
        }

        if (possibleNumeric(cut)) {
            cut <- asNumeric(cut)
        }

        if (any(duplicated(cut))) {
            stopError("Cut values should be unique.")
        }

        if (is.null(values)) {
            values <- seq(length(cut) + 1)
        }
        else {
            if (length(values) == 1 & is.character(values)) {
                values <- gsub(
                    "\n|\t", "", gsub(
                        "'", "", gsub(
                            ")", "", gsub(
                                "c(", "", values, fixed = TRUE
                            )
                        )
                    )
                )

                values <- trimstr(unlist(strsplit(values, split = ",")))

                if (length(values) == 1) {
                    values <- trimstr(unlist(strsplit(values, split = ";")))
                }
            }

            if (length(values) == length(cut) + 1) {
                as.numeric.result <- possibleNumeric(values)
                if (as.numeric.result) {
                    values <- asNumeric(values)
                }
            }
            else {
                stopError(
                    paste0(
                        "There should be ", length(cut) + 1,
                        " values for ", length(cut), " cut value",
                        ifelse(length(cut) == 1, "", "s"), "."
                    )
                )
            }
        }

        if (!is.null(factor.labels)) {
            if (length(factor.labels) != length(values)) {
                stopError("The number of labels should be equal to the number of recodings.")
            }
        }

        if (is.factor(x)) {
            lx <- levels(x)
            minx <- lx[1]
            maxx <- lx[length(lx)]

            if (is.numeric(cut)) {
                insidex <- FALSE
            }
            else {
                insidex <- all(is.element(cut, lx))
            }
        }
        else {
            if (is.character(x) & is.numeric(cut)) {
                insidex <- FALSE
            }
            else if (is.character(x) & is.character(cut)) {
                insidex <- is.element(cut, x[!is.na(x)])
            } else {
                insidex <- cut >= min(x, na.rm = TRUE) & cut <= max(x, na.rm = TRUE)
            }
        }

        if (!all(insidex)) {
            message <- "Cut value(s) outside the input vector."
            stopError(message)
        }

        if (is.factor(x)) {
            nx <- as.numeric(x)
            nlx <- seq(length(lx))
            nc <- match(cut, lx)
            temp <- rep(values[1], length(x))
            for (i in seq(length(cut))) {
                temp[nx > nc[i]] = values[i + 1]
            }
        }
        else {
            nax <- which(is.na(x))
            temp <- rep(values[1], length(x))
            for (i in seq(length(cut))) {
                temp[x > cut[i]] = values[i + 1]
            }
            if (length(nax) > 0) {
                temp[nax] <- NA
            }
        }

        if (!is.null(factor.labels) && length(factor.labels) == 0 && is.numeric(cut)) {
            factor.labels <- values
        }
    }

    if (as.factor.result) {
        if (length(factor.levels) == 0) {
            factor.levels <- sort(unique(na.omit(temp)))
        }

        if (!is.null(factor.labels) && length(factor.labels) == 0) {
            factor.labels <- factor.levels
        }

        temp <- factor(
            temp,
            levels = factor.levels,
            labels = factor.labels,
            ordered = factor.ordered
        )
    }
    else if (as.numeric.result) {
        if (possibleNumeric(temp)) {
            temp <- asNumeric(temp)
        }

        if (!is.null(factor.labels)) {
            names(values) <- factor.labels
            attr(temp, "labels") <- values
        }
    }

    return(temp)
}
