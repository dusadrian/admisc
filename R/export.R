
export <- function (what, ...) {
    UseMethod ("export")
}


`export.default` <- function (what, ...) {
    # do nothing
    return(NULL)
}


`export.data.frame` <- function(what, ...) {

    dots <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]

    caseid <- "cases"
    if (any(names(dots) == "caseid")) {
        caseid <- dots[["caseid"]]
        Call[["caseid"]] <- NULL
    }

    if (any(rownames(what) != seq(nrow(what)))) {
        if (all(colnames(what) != caseid)) {
            what <- cbind("cases" = rownames(what), what)
            names(what)[1] <- caseid
        }
    }

    Call[["x"]] <- what
    Call[["what"]] <- NULL

    if (any(names(dots) == "sep")) {
        if (dots[["sep"]] == "tab") {
            dots[["sep"]] <- "\t"
        }
        Call[["sep"]] <- dots[["sep"]]
    }
    else {
        Call[["sep"]] <- ","
    }

    if (any(names(dots) == "col.names")) {
        Call[["col.names"]] <- dots[["col.names"]]
    }

    if (any(names(dots) == "row.names")) {
        message("The argument 'row.names' is always set to FALSE, by default.")
    }

    Call[["row.names"]] <- FALSE

    do.call("write.table", Call)

}

`export.list` <- function(what, ...) {
    dots <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]

    DDIwR <- eval(parse(text = "requireNamespace('DDIwR', quietly = TRUE)"))

    if (!DDIwR) {
        stopError("Package DDIwR needs to be installed.")
    }

    if (is.null(what$.extra)) {
        return(NULL)
    }

    names(Call)[1] <- "codeBook"

    eval(parse(text = "do.call('exportCodebook', Call)"))
}