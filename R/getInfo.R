`getInfo` <- function(data) {
    
    if (is.matrix(data)) {
        data <- as.data.frame(data)
    }

    dc.code <- unique(unlist(lapply(data, function(x) {
        if (is.numeric(x)) {
            return(x[x < 0])
        }
        else {
            return(as.character(x[is.element(x, c("-", "dc"))]))
        }
    })))
    
    if (length(dc.code) > 1) {
        stopError("Multiple \"don't care\" codes found.")
    }

    if (length(dc.code) > 0) {
        colnms <- colnames(data)

        data[] <- lapply(data, function(x) {
            # to make sure that any factor is character
            x <- as.character(x)
            
            # otherwise replacement was not possible
            x[x == dc.code] <- -1
            
            if (possibleNumeric(x)) {
                x <- asNumeric(x)
            }

            return(x)
        })

        colnames(data) <- colnms
    }
    
    fuzzy.cc <- logical(ncol(data))
    hastime <- logical(ncol(data))
    factor <- unlist(lapply(data, is.factor))
    declared <- unlist(lapply(data, function(x) inherits(x, "declared")))
    
    pN <- unlist(lapply(data, possibleNumeric))
    
    for (i in seq(ncol(data))) {
        if (pN[i]) {
            fuzzy.cc[i] <- any(na.omit(data[, i]) %% 1 > 0)
            if (!fuzzy.cc[i] & !any(is.na(data[, i]))) {
                copy.cc <- data[, i]
                if (any(na.omit(copy.cc) < 0)) {
                    hastime[i] <- TRUE
                    copy.cc[copy.cc < 0] <- max(copy.cc) + 1
                    data[, i] <- copy.cc
                }
            }
        }
    }
    
    # the data MUST begin with 0 and MUST be incremented by 1 for each level...!
    # perhaps trying something like
    # apply(data, 2, function(x) length(unique(x))) + 1
    # noflevels <- apply(data, 2, max) + 1
    # noflevels[noflevels == 1] <- 2
    # noflevels[fuzzy.cc] <- 2
    # noflevels <- as.integer(noflevels)
    
    noflevels <- getLevels(data)
    attributes(noflevels) <- NULL

    factor <- factor & !hastime

    categories <- list()
    columns <- colnames(data)
    
    if (any(factor | declared)) {
        for (i in which(factor | declared)) {
            if (factor[i]) {
                values <- seq(noflevels[i]) - 1
                names(values) <- levels(data[, i])
                data[, i] <- as.numeric(data[, i]) - 1
                categories[[columns[i]]] <- values
            }
            else {
                values <- seq(noflevels[i]) - 1
                x <- data[, i]
                labels <- attr(x, "labels")
                if (is.null(labels)) {
                    stopError("Declared columns should have labels for all values.")
                }
                else {
                    if (length(labels) != noflevels[i]) {
                        stopError("All values should have declared labels.")
                    }
                }
                attributes(x) <- NULL
                data[, i] <- recode(x, paste(sort(labels), values, sep = "=", collapse = ";"))
                names(values) <- names(labels)
                categories[[columns[i]]] <- values
                attr(categories, "labels") <- labels
            }
        }
    }
    
    return(
        list(
            data = data,
            fuzzy.cc = fuzzy.cc,
            hastime = hastime,
            factor = factor,
            declared = declared,
            categories = categories,
            dc.code = dc.code,
            noflevels = noflevels
        )
    )
}
