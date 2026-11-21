#' @export
`getLevels` <- function(data) {
    data <- as.data.frame(data)

    # Ordinary numeric columns need no common matrix representation.
    if (all(vapply(data, function(x) is.numeric(x) && !is.object(x) &&
            is.null(dim(x)) && length(x) > 0L, logical(1)))) {
        return(unname(vapply(data, function(x) {
            if (any(x %% 1 > 0, na.rm = TRUE)) return(2)
            n <- if (anyNA(x)) length(unique(x)) else max(x) + 1
            if (n == 1) 2 else as.double(n)
        }, numeric(1))))
    }

    pN <- sapply(data, possibleNumeric)

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
