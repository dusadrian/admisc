#' Add cases from a second data frame
#'
#' Appends the rows of a second data frame, matching variables by name and
#' filling absent variables with missing values.
#'
#' @param x,y Data frames. Rows from \code{x} precede rows from \code{y}.
#' @param keep Which variables to keep: \code{"common"} (the default),
#'   \code{"all"}, \code{"x"}, or \code{"y"}.
#' @param add.x,add.y Additional variables unique to \code{x} or \code{y},
#'   supplied as comma-separated strings or character vectors.
#' @param exclude Variables to leave out, supplied as a comma-separated string
#'   or a character vector. \code{NULL} excludes no variables.
#'
#' @details
#' \code{"common"} keeps variables present in both data frames.
#' \code{"all"} keeps variables present in either data frame.
#' \code{"x"} keeps the variables of \code{x}, while \code{"y"}
#' keeps the variables of \code{y}. Variables follow the first data frame's
#' order, except when \code{keep = "y"}. With \code{keep = "all"},
#' variables found only in \code{y} are appended in their original order.
#' If there are no common variables, the default produces an error; choose
#' \code{"all"}, \code{"x"}, or \code{"y"} to retain variables explicitly.
#'
#' \code{add.x} and \code{add.y} add variables unique to their respective
#' inputs after applying \code{keep}. They also work without common variables.
#' Exclusions are applied last, after these additions. An excluded
#' name must exist in at least one input. A single exclusion string matching
#' an existing variable name is treated as that name, including any commas.
#' Otherwise, a single string is split at commas and surrounding whitespace
#' is removed. A character vector of length greater than one supplies exact
#' variable names.
#'
#' Missing columns use the corresponding source column's type and attributes.
#' Shared columns are combined using \code{\link[base]{rbind}}, including its
#' usual type coercion and factor-level handling. Inputs must have unique,
#' nonempty variable names, and at least one variable must be retained.
#'
#' @return A data frame containing the rows of \code{x} followed by those of
#'   \code{y}. The input data frames are unchanged.
#' @author Adrian Dusa
#' @examples
#' first <- data.frame(id = 1:2, age = c(25, 40))
#' second <- data.frame(score = c(8, 9), id = 3:4)
#'
#' addCases(first, second)
#' addCases(first, second, keep = "all")
#' addCases(first, second, keep = "x")
#' addCases(first, second, keep = "y")
#'
#' addCases(first, second, add.x = "age")
#' addCases(first, second, add.y = "score")
#'
#' combined <- addCases(first, second, keep = "all", exclude = "age")
#' @export
`addCases` <- function(x, y, keep = "common", exclude = NULL, add.x = NULL, add.y = NULL) {
    if (!is.data.frame(x) || !is.data.frame(y)) {
        stopError("Both inputs must be data frames.")
    }
    keep <- match.arg(keep, c("common", "all", "x", "y"))

    namesx <- names(x)
    namesy <- names(y)
    if (anyNA(c(namesx, namesy)) || any(!nzchar(c(namesx, namesy))) ||
        anyDuplicated(namesx) || anyDuplicated(namesy)) {
        stopError("Each data frame must have unique, nonempty variable names.")
    }

    available <- union(namesx, namesy)
    variables <- switch(keep,
        common = intersect(namesx, namesy),
        all = available,
        x = namesx,
        y = namesy
    )

    readNames <- function(value, choices, argument) {
        if (is.null(value)) return(character())
        if (!is.character(value) || anyNA(value)) {
            stopError(paste0("Argument '", argument, "' must contain variable names."))
        }
        if (length(value) == 1L && !value %in% choices) {
            value <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
            value <- value[nzchar(value)]
        }
        unknown <- setdiff(value, choices)
        if (length(unknown)) {
            stopError(paste0("Invalid variables in '", argument, "': ", paste(unknown, collapse = ", ")))
        }
        value
    }

    addedx <- readNames(add.x, setdiff(namesx, namesy), "add.x")
    addedy <- readNames(add.y, setdiff(namesy, namesx), "add.y")
    variables <- union(variables, union(addedx, addedy))
    if (keep == "common" && !length(variables)) {
        stopError('The data frames have no common variables. Use add.x, add.y or another keep option.')
    }
    variables <- setdiff(variables, readNames(exclude, available, "exclude"))
    # Preserve source column order, including when extra variables are added.
    order <- if (keep == "y") union(namesy, namesx) else available
    variables <- order[order %in% variables]

    if (!length(variables)) {
        stopError("No variables remain. Choose another 'keep' option or change 'exclude'.")
    }

    # Use ordinary data-frame indexing for subclasses such as data.table.
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    x <- x[, intersect(variables, namesx), drop = FALSE]
    y <- y[, intersect(variables, namesy), drop = FALSE]

    missingx <- setdiff(variables, namesx)
    missingy <- setdiff(variables, namesy)

    # Integer row indices give exactly the required number of missing rows.
    # The column types come from the other data frame, not from the indices.
    if (length(missingx)) {
        x[missingx] <- y[rep(NA_integer_, nrow(x)), missingx, drop = FALSE]
    }
    if (length(missingy)) {
        y[missingy] <- x[rep(NA_integer_, nrow(y)), missingy, drop = FALSE]
    }

    rbind(x[, variables, drop = FALSE], y[, variables, drop = FALSE])
}
