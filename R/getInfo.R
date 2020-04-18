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
        cat("\n")
        stop(simpleError("Multiple \"don't care\" codes found.\n\n"))
    }

    if (length(dc.code) > 0) {
        colnms <- colnames(data)

        data[] <- lapply(data, function(x) {
            # to make sure that any factor is character
            x <- as.character(x)
            
            # otherwise replacement was not possible
            x[x == dc.code] <- -1
            
            return(asNumeric(x))
        })

        colnames(data) <- colnms
    }
    
    fuzzy.cc <- logical(ncol(data))
    hastime <- logical(ncol(data))
    
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
    
    return(list(data = data, fuzzy.cc = fuzzy.cc, hastime = hastime, dc.code = dc.code, noflevels = as.numeric(noflevels)))
}
