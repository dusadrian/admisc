`checkValid` <- function(
    expression = "", snames = "", data = NULL, labels = list()
) {

    if (identical(snames, "")) {
        stopError("The expression cannot be verified without <snames>.")
    }

    allnames <- splitstr(snames)
    
    if (!is.null(data)) {
        allnames <- colnames(data)
        infodata <- getInfo(data)
        if (any(infodata$factor)) {
            allnames <- c(allnames, names(unlist(infodata$labels)))
        }
    }
    else if (length(labels) > 0) {
        allnames <- c(allnames, names(unlist(labels)))
    }
        
    expression <- replaceText(
        expression,
        allnames,
        rep("", length(allnames))
    )

    if (any(grepl(":alpha:", expression))) { # is it not [:alpha:] ???
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
