`checkValid` <- function(
    expression = "", snames = "", data = NULL
) {

    if (identical(snames, "")) {
        stopError("The expression cannot be verified without <snames>.")
    }
    
    if (!is.null(data)) {
        allnames <- colnames(data)
        infodata <- getInfo(data)
        if (any(infodata$factor)) {
            allnames <- c(allnames, names(unlist(infodata$factors)))
        }
        
        expression <- replaceText(
            expression,
            allnames,
            rep("", length(allnames))
        )

        if (grepl(":alpha:", expression)) {
            stopError("Part(s) of the expression not found in the data")
        }
    }
    else {
        expression <- replaceText(
            expression,
            snames,
            rep("", length(snames))
        )

        if (grepl(":alpha:", expression)) {
            stopError("Part(s) of the expression not found in the <snames> argument")
        }
    }
}
