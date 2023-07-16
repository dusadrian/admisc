`getLevels` <- function(data) {
    data <- as.data.frame(data)
    
    colnames <- paste("V", ncol(data), sep = ".")
    
    pN <- unlist(lapply(data, possibleNumeric))
    
    noflevels <- rep(NA, ncol(data))
    ulevels <- rep(NA, ncol(data))

    noflevels[pN] <- apply(
        data[, pN, drop = FALSE],
        2,
        # as.numeric() still makes sense, for instance as.factor(c(0,1,1,0,1))
        # which IS possibly numeric, yet still doesn't cope with max()
        function(x) max(as.numeric(x))
    ) + 1

    ulevels <- apply(
        data,
        2,
        function(x) {
            return(length(unique(x)))
        }
    )

    noflevels[is.na(noflevels)] <- ulevels[is.na(noflevels)]


    factor <- unlist(lapply(data, is.factor))
    declared <- unlist(lapply(data, function(x) inherits(x, "declared")))

    noflevels[pN][
        apply(
            data[, pN, drop = FALSE],
            2,
            function(x) any(as.numeric(x) %% 1 > 0)
        )
    ] <- 2

    if (any(factor | declared)) {
        noflevels[factor | declared] <- pmin(noflevels[factor | declared], ulevels[factor | declared])
    }

    noflevels[noflevels == 1] <- 2
    return(noflevels)
}
