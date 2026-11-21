#' Evaluate an expression in a data environment
#'
#' Similar to the base function \code{\link[base]{with}()}, this function
#' evaluates an expression using the names in the supplied data. It can also
#' evaluate the same expression separately for each group.
#'
#' @name using
#' @rdname using
#' @aliases using.data.frame
#' @rawRd
#' \usage{
#' using(data, expr, split.by = NULL, ..., .simplify = TRUE)
#' }
#'
#' \arguments{
#'     \item{data}{A data frame (including a tibble or a \code{data.table}),
#'         a matrix, a named list, a named atomic vector, or an environment.}
#'     \item{expr}{An R expression, for instance \code{mean(Age)}.}
#'     \item{split.by}{One or more grouping variables. For instance, use
#'         \code{Area} for a single variable, or \code{Area & Gender} for
#'         both. A character vector of column names also works. A grouping
#'         variable supplied separately must have one value per row. Use a
#'         factor for a grouping variable supplied as character values.}
#'     \item{...}{A single assignment can also be written as a named argument,
#'         for example \code{using(DF, total = men + women)}.}
#'     \item{.simplify}{If possible, collect the group results in a matrix.
#'         Set this to \code{FALSE} to keep a list with one result per group.}
#' }
#'
#' \details{
#' Given a data frame \code{DF}, the call \code{using(DF, mean(Age))} is
#' another way to write \code{mean(DF$Age)}. If there is also a variable
#' called \code{Age} outside the data frame, the column in \code{DF} takes
#' precedence. The same idea applies to the columns of a matrix and the
#' elements of a named list or vector. Other names are looked up where
#' the function was called. Warnings, messages and interrupts are handled
#' as usual.
#'
#' For instance, if \code{counts <- c(men = 12, women = 15)}, the call
#' \code{using(counts, men + women)} returns 27. If a name occurs more than once,
#' the first occurrence is used. Elements with empty or missing names are
#' skipped when looking up a name. An assignment made with \code{<-} is
#' local to this calculation. To save a change in the vector, use
#' \code{\link{inside}()}. For an environment, the expression runs directly
#' in that environment, so assignments made there persist.
#'
#' With \code{split.by = Area}, the mean age is calculated separately for
#' each area. Groups follow the order of the factor levels. Within each group,
#' the rows keep their original order. Only groups present in the data are
#' used. Cases with missing values on a grouping variable are excluded,
#' including values or ranges declared as missing. A list can be split if
#' its columns have distinct, nonempty names and the same number of rows.
#' Named atomic vectors and environments are evaluated as a whole.
#'
#' When working with a data frame, \code{.data} refers to the whole data
#' frame, or to the rows of the current group when using \code{split.by}.
#' For instance, a model can be fitted with \code{data = .data}. This also
#' works for a list split into groups. If a column is itself called \code{.data},
#' it can be selected with \code{.data[[".data"]]}.
#'
#' For a direct call to \code{lm()} or \code{glm()} with a stored formula,
#' each model uses the rows of its group. With a formula such as
#' \code{Age ~ .}, the dot leaves out the grouping variables, which are
#' constant within the group. A grouping variable can still be included by
#' name. An explicit \code{data} argument always takes precedence.
#' }
#'
#' \value{
#' The value of the expression. With \code{split.by}, the result is a matrix
#' if each group returns a compatible vector of the same length. For example,
#' \code{mean(Age)} gives one row per group and one column of means. If the
#' vectors have names, their elements are matched by name.
#'
#' Other results, such as frequency tables, fitted models and \code{NULL},
#' are collected in a list. The grouping values are available in
#' \code{attr(result, "group.keys")}, with one row per group. Numeric
#' summaries are rounded for printing.
#' }
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' # calculate the total for one area
#' counts <- c(men = 12, women = 15)
#' using(counts, men + women)
#'
#' # same calculation for each row of a matrix
#' m <- cbind(men = c(12, 9), women = c(15, 11))
#' using(m, men + women)
#'
#' # same with the two counts stored in an environment
#' e <- list2env(as.list(counts))
#' using(e, men + women)
#'
#' set.seed(123)
#' DF <- data.frame(
#'     Area = factor(sample(c("Rural", "Urban"), 123, replace = TRUE)),
#'     Gender = factor(sample(c("Female", "Male"), 123, replace = TRUE)),
#'     Age = sample(18:90, 123, replace = TRUE),
#'     Children = sample(0:5, 123, replace = TRUE)
#' )
#'
#'
#' # table of frequencies for Gender
#' table(DF$Gender)
#'
#' # same with
#' using(DF, table(Gender))
#'
#' # same, but split by Area
#' using(DF, table(Gender), split.by = Area)
#'
#' # calculate the mean age by gender
#' using(DF, mean(Age), split.by = Gender)
#'
#' # same, but select cases from the urban area
#' using(subset(DF, Area == "Urban"), mean(Age), split.by = Gender)
#'
#' # mean age by gender and area
#' using(DF, mean(Age), split.by = Area & Gender)
#'
#' # same with
#' using(DF, mean(Age), split.by = c(Area, Gender))
#'
#' # average number of children by Area
#' using(DF, mean(Children), split.by = Area)
#'
#' # one fitted model per area, using the current group's data
#' fits <- using(DF, lm(Age ~ Children, data = .data), split.by = "Area")
#' lapply(fits, coef)
#'
#' # frequency tables by Area
#' using(DF, table(Children), split.by = Area)
#' }
#'
#' \keyword{functions}
NULL

#' @export
`using` <- function(data, expr, split.by = NULL, ..., .simplify = TRUE) {
    UseMethod("using")
}

# A bare wrapper formal can forward an unevaluated expression and its caller.
# A data column of the same name always wins, as in with().
usingArgument <- function(code, caller, columns) {
    while (is.symbol(code) && !as.character(code) %in% columns) {
        frames <- sys.frames()
        frame <- which(vapply(frames, identical, logical(1), caller))
        if (!length(frame)) break
        frame <- frame[length(frame)]
        name <- as.character(code)
        fun <- sys.function(frame)
        if (!name %in% names(formals(fun))) break
        actual <- match.call(fun, sys.call(frame), expand.dots = FALSE)
        if (!name %in% names(actual)) break
        binding <- eval(call("substitute", code, caller), baseenv())
        if (!identical(binding, actual[[name]])) break
        code <- actual[[name]]
        parent <- sys.parents()[frame]
        caller <- if (parent) frames[[parent]] else globalenv()
    }
    list(code = code, caller = caller)
}

usingSplit <- function(code, data, caller) {
    if (is.call(code) && (identical(code[[1L]], quote(c)) || identical(code[[1L]], quote(`&`)))) {
        parts <- lapply(as.list(code)[-1L], usingSplit, data = data, caller = caller)
        if (!length(parts) || any(lengths(parts) == 0L)) stopError("Empty split specification.")
        return(do.call(c, unname(parts)))
    }
    if (is.symbol(code)) {
        name <- as.character(code)
        if (name %in% names(data)) return(stats::setNames(list(data[[name]]), name))
        if (!exists(name, caller, inherits = TRUE)) stopError(paste("Unknown split variable", name))
        value <- get(name, caller, inherits = TRUE)
        if (is.null(value)) return(NULL)
        if (!is.character(value)) {
            return(stats::setNames(list(value), name))
        }
        code <- value
    }
    if (!is.character(code) || !length(code) || anyNA(code) || !all(code %in% names(data))) {
        stopError("Specify split.by as a column name, column names in c(), or grouping variables joined by &.")
    }
    as.list(data)[code]
}

usingFactor <- function(x) {
    if (inherits(x, "declared") || inherits(x, "haven_labelled")) {
        labels <- attr(x, "labels", exact = TRUE)
        missing <- attr(x, "na_values", exact = TRUE)
        range <- attr(x, "na_range", exact = TRUE)
        attributes(x) <- NULL
        exclude <- is.na(x) | x %in% missing
        if (length(range)) exclude <- exclude | (x >= min(range) & x <= max(range))
        x[exclude] <- NA
        values <- sort(unique(x))
        display <- as.character(values)
        labelled <- match(values, labels)
        display[!is.na(labelled)] <- names(labels)[labelled[!is.na(labelled)]]
        # Different codes can share a display label. Keep their groups distinct.
        return(factor(x, levels = values, labels = make.unique(display)))
    }
    as.factor(x)
}

usingMatrix <- function(values, expr) {
    if (!length(values)) return(NULL)
    first <- values[[1L]]
    schema <- names(first)
    plain <- function(x) {
        is.atomic(x) && length(x) > 0L &&
            (is.null(attributes(x)) || identical(names(attributes(x)), "names"))
    }
    if (!plain(first)) return(NULL)
    for (i in seq_along(values)) {
        x <- values[[i]]
        if (!plain(x) || length(x) != length(first)) return(NULL)
        if (typeof(x) != typeof(first) && !(is.numeric(first) && is.numeric(x) &&
            !is.complex(first) && !is.complex(x))) return(NULL)
        if (!identical(names(x), schema)) {
            if (is.null(schema) || anyNA(schema) || any(!nzchar(schema)) ||
                anyDuplicated(schema) || !setequal(names(x), schema)) return(NULL)
            values[[i]] <- x[schema]
        }
    }
    result <- do.call(rbind, unname(values))
    if (is.null(schema)) {
        if (length(first) == 1L) {
            head <- if (is.call(expr)) expr[[1L]] else expr
            schema <- paste(deparse(head), collapse = " ")
        } else if (is.call(expr) && identical(expr[[1L]], quote(c)) && length(expr) == length(first) + 1L) {
            schema <- vapply(as.list(expr)[-1L], function(x) paste(deparse(x), collapse = " "), character(1))
        } else {
            schema <- rep(" ", length(first))
        }
    }
    colnames(result) <- schema
    result
}

# Prepare the delayed assignments once. Evaluate them in a fresh frame for
# each group, so returned closures always retain their own row indices.
usingMaskBindings <- function(data) {
    columns <- names(data)
    ordinary <- identical(class(data), "data.frame")
    bindings <- lapply(which(columns != ".data"), function(j) {
        column <- if (ordinary) .subset2(data, j)
        slice <- if (ordinary && !is.object(column) && is.null(dim(column))) {
            substitute(.subset2(data, j)[index], list(j = j))
        } else {
            substitute(data[index, field, drop = FALSE][[1L]], list(field = columns[j]))
        }
        substitute(delayedAssign(field, value, eval.env = frame, assign.env = mask),
            list(field = columns[j], value = slice))
    })
    as.call(c(list(quote(`{`)), bindings))
}

usingGroupMask <- function(data, index, caller, bindings) {
    force(index)
    frame <- environment()
    mask <- new.env(parent = caller)
    delayedAssign(".data", data[index, , drop = FALSE], eval.env = frame, assign.env = mask)
    eval(bindings, frame)
    mask
}

# Supply current observations to direct base regression calls using an external
# formula. Literal formulas already capture the evaluation environment.
usingModelCall <- function(expr, data, by, caller) {
    if (!is.call(expr)) return(expr)
    head <- expr[[1L]]
    if (is.symbol(head) && as.character(head) %in% c("lm", "glm")) {
        name <- as.character(head)
        if (name %in% names(data) && is.function(data[[name]])) return(expr)
        fun <- get0(name, envir = caller, mode = "function", inherits = TRUE)
        if (!identical(fun, getExportedValue("stats", name))) return(expr)
    } else if (identical(head, quote(stats::lm)) || identical(head, quote(stats::glm))) {
        fun <- getExportedValue("stats", as.character(head[[3L]]))
    } else return(expr)
    if (any(vapply(as.list(expr)[-1L], identical, logical(1), quote(...)))) return(expr)
    args <- match.call(fun, expr, expand.dots = FALSE)
    if ("data" %in% names(args) || is.null(args$formula)) return(expr)
    formula <- args$formula
    if (is.call(formula) && identical(formula[[1L]], quote(`~`))) {
        vars <- all.vars(formula)
        if (!"." %in% vars) return(expr)
        # A grouping factor is constant within its group. Include it only if
        # mentioned explicitly in the formula.
        cols <- setdiff(names(data), setdiff(by, vars))
        expr$data <- call("[", quote(.data), cols)
    } else {
        expr$data <- quote(.data)
    }
    expr
}

#' @export
`using.default` <- function(data, expr, split.by = NULL, ..., .simplify = TRUE) {
    caller <- parent.frame()
    code <- substitute(expr)
    dots <- as.list(substitute(list(...)))[-1L]
    if (missing(expr)) {
        if (length(dots) != 1L || is.null(names(dots)) || !nzchar(names(dots)[1L])) {
            stopError("Missing or ambiguous expression.")
        }
        code <- call("<-", as.name(names(dots)[1L]), dots[[1L]])
    } else if (length(dots)) {
        stopError("Unused arguments to using(). Specify grouping with split.by.")
    }
    if (is.matrix(data)) {
        data <- as.data.frame(data, stringsAsFactors = FALSE, optional = !is.null(colnames(data)))
    }
    columns <- if (is.environment(data)) ls(data, all.names = TRUE) else names(data)
    arg <- usingArgument(code, caller, columns)
    by <- usingArgument(substitute(split.by), caller, columns)
    vars <- if (!is.null(by$code)) usingSplit(by$code, data, by$caller)
    if (is.atomic(data) && !is.null(data) && is.null(dim(data))) {
        if (is.null(columns)) stopError("using() requires names for an atomic vector.")
        if (!is.null(vars)) stopError("Named atomic vectors do not support split.by.")
        indices <- which(vectorBindingNames(columns))
        data <- lapply(indices, function(i) data[[i]])
        names(data) <- columns[indices]
    }
    if (is.null(vars)) {
        if (is.data.frame(data)) {
            mask <- as.list(data)
            mask[[".data"]] <- data
        } else mask <- data
        return(eval(arg$code, envir = mask, enclos = arg$caller))
    }
    if (!is.logical(.simplify) || length(.simplify) != 1L || is.na(.simplify)) {
        stopError(".simplify must be TRUE or FALSE.")
    }
    if (!is.list(data) || is.null(columns) || anyNA(columns) || any(!nzchar(columns)) ||
        anyDuplicated(columns)) stopError("Grouped evaluation requires uniquely named columns.")
    if (!is.data.frame(data)) {
        sizes <- vapply(data, NROW, integer(1))
        if (length(unique(sizes)) != 1L) stopError("All grouped list columns must have the same row count.")
        data <- structure(as.list(data), class = "data.frame", row.names = .set_row_names(sizes[1L]))
    } else data <- as.data.frame(data)

    if (anyDuplicated(names(vars))) stopError("Duplicate grouping variables.")
    factors <- lapply(vars, function(x) {
        if (!is.atomic(x) || !is.null(dim(x)) || length(x) != nrow(data)) {
            stopError("Each grouping variable must be a vector with one value per row.")
        }
        usingFactor(x)
    })
    codes <- lapply(factors, function(x) {
        code <- as.integer(x)
        if (anyNA(levels(x))) code[is.na(levels(x)[code])] <- NA_integer_
        code
    })
    ordered <- do.call(order, c(unname(codes), list(na.last = NA, method = "radix")))
    first <- rep(FALSE, length(ordered))
    if (length(ordered)) {
        first[1L] <- TRUE
        for (code in codes) {
            sorted <- code[ordered]
            first[-1L] <- first[-1L] | sorted[-1L] != sorted[-length(sorted)]
        }
    }
    rows <- split(ordered, cumsum(first))
    representatives <- ordered[first]
    keys <- as.data.frame(lapply(vars, `[`, representatives), optional = TRUE)
    labels <- matrix("", length(rows), length(vars), dimnames = list(NULL, names(vars)))
    for (j in seq_along(factors)) labels[, j] <- as.character(factors[[j]][representatives])
    ids <- if (length(rows)) make.unique(apply(labels, 1L, paste, collapse = ",")) else character()
    expr <- usingModelCall(arg$code, data, names(vars), arg$caller)
    printed <- is.call(expr) && identical(expr[[1L]], quote(`{`)) &&
        any(vapply(as.list(expr)[-1L], function(x) is.call(x) && identical(x[[1L]], quote(print)), logical(1)))
    values <- vector("list", length(rows))
    bindings <- usingMaskBindings(data)
    visible <- TRUE
    for (i in seq_along(rows)) {
        mask <- usingGroupMask(data, rows[[i]], arg$caller, bindings)
        label <- paste(labels[i, ], collapse = ", ")
        if (printed) cat("\n", label, "\n", strrep("-", nchar(label)), "\n", sep = "")
        tmp <- tryCatch(withVisible(eval(expr, mask, arg$caller)), error = function(e) {
            attr(e, "group.keys") <- keys[i, , drop = FALSE]
            if (identical(class(e)[1L], "simpleError")) {
                e$parent <- e
                e$message <- paste0("Group ", label, ": ", conditionMessage(e))
            }
            stop(e)
        })
        values[i] <- list(tmp$value)
        visible <- tmp$visible
        if (printed && visible) print(tmp$value)
    }
    if (printed) cat("\n")
    result <- if (.simplify) usingMatrix(values, expr)
    if (is.null(result)) {
        result <- stats::setNames(values, ids)
    } else rownames(result) <- ids
    attr(result, "group.keys") <- keys
    attr(result, "split") <- if (ncol(labels) == 1L) unname(labels[, 1L]) else labels
    class(result) <- c("admisc_fobject", class(result))
    if (visible && !printed) result else invisible(result)
}

#' @export
using.data.frame <- using.default
#' @export
using.matrix <- using.default

#' @export
`[.admisc_fobject` <- function(x, i, j, ..., drop = FALSE) {
    keys <- attr(x, "group.keys", exact = TRUE)
    labels <- attr(x, "split", exact = TRUE)
    class(x) <- setdiff(class(x), "admisc_fobject")
    ids <- if (is.matrix(x)) rownames(x) else names(x)
    positions <- stats::setNames(seq_len(NROW(x)), ids)
    if (is.matrix(x)) {
        if (nargs() - as.integer(!missing(drop)) == 2L && !missing(i)) return(x[i])
        value <- x[i, j, drop = drop]
        if (!is.matrix(value)) return(value)
    } else value <- x[i]
    if (!missing(i)) positions <- positions[i]
    if (!is.null(keys)) attr(value, "group.keys") <- keys[positions, , drop = FALSE]
    attr(value, "split") <- if (is.matrix(labels)) labels[positions, , drop = FALSE] else labels[positions]
    class(value) <- c("admisc_fobject", class(value))
    value
}
