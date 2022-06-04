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
            if (!inherits(x, "declared")) {
                x <- as.character(x)
                
                # otherwise replacement was not possible
                x[x == dc.code] <- -1
                
                if (possibleNumeric(x)) {
                    x <- asNumeric(x)
                }                
            }

            return(x)
        })

        colnames(data) <- colnms
    }
    
    fuzzy.cc <- logical(ncol(data))
    hastime <- logical(ncol(data))
    factor <- sapply(data, is.factor)
    declared <- sapply(data, function(x) inherits(x, "declared"))
    
    pN <- sapply(data, possibleNumeric)
    
    for (i in seq(ncol(data))) {
        if (pN[i] & !declared[i]) {
            copy.cc <- asNumeric(data[, i])

            fuzzy.cc[i] <- any(na.omit(copy.cc) %% 1 > 0)
            if (!fuzzy.cc[i] & !any(is.na(copy.cc))) {
                if (any(na.omit(copy.cc) < 0)) {
                    hastime[i] <- TRUE
                    copy.cc[copy.cc < 0] <- max(copy.cc) + 1
                }
            }
            
            data[, i] <- copy.cc
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
                categories[[columns[i]]] <- levels(data[, i])
                data[, i] <- as.numeric(data[, i]) - 1
            }
            else {
                
                x <- data[, i]
                labels <- attr(x, "labels", exact = TRUE)

                if (is.null(labels)) {
                    stopError("Declared columns should have labels for all values.")
                }
                else {
                    if (length(labels) != noflevels[i]) {
                        stopError("All values should have declared labels.")
                    }
                }

                attributes(x) <- NULL
                data[, i] <- recode(x, paste(sort(labels), seq(noflevels[i]) - 1, sep = "=", collapse = ";"))
                categories[[columns[i]]] <- names(sort(labels))
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
