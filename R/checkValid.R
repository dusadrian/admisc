`checkValid` <- function(
    expression = "", snames = "", data = NULL, categories = list()
) {

    if (identical(snames, "")) {
        stopError("The expression cannot be verified without <snames>.")
    }

    allnames <- splitstr(snames)
    
    if (!is.null(data)) {
        allnames <- colnames(data)
        infodata <- getInfo(data)
        if (any(infodata$factor)) {
            allnames <- c(allnames, names(unlist(infodata$categories)))
        }
    }
    else if (length(categories) > 0) {
        allnames <- c(allnames, names(unlist(categories)))
    }
        
    expression <- replaceText(
        expression,
        allnames,
        rep("", length(allnames))
    )

    if (grepl(":alpha:", expression)) {
        stopError(
            sprintf(
                "Part(s) of the expression not found in the %s.",
                ifelse(
                    is.null(data),
                    "<snames> argument",
                    "data"
                )
            )
        )
    }
}
