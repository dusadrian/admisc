`export` <-
function(x, file = "", ...) {
    
    export.args <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]
    
    caseid <- "cases"
    if (any(names(export.args) == "caseid")) {
        caseid <- export.args[["caseid"]]
        Call[["caseid"]] <- NULL
    }
    
    if (!missing(x)) {
        if (is.data.frame(x) | is.matrix(x)) {
            if (any(rownames(x) != seq(nrow(x)))) {
                if (all(colnames(x) != caseid)) {
                    x <- cbind("cases" = rownames(x), x)
                    names(x)[1] <- caseid
                }
            }
        }
    }
    
    Call[["x"]] <- x
    
    if (any(names(export.args) == "sep")) {
        if (export.args[["sep"]] == "tab") {
            export.args[["sep"]] <- "\t"
        }
        Call[["sep"]] <- export.args[["sep"]]
    }
    else {
        Call[["sep"]] <- ","
    }
    
    if (any(names(export.args) == "col.names")) {
        Call[["col.names"]] <- export.args[["col.names"]]
    }
    
    if (any(names(export.args) == "row.names")) {
        message("The argument 'row.names' is always set to FALSE, by default.")
    }
    
    Call[["row.names"]] <- FALSE
    
    do.call("write.table", Call)
}
