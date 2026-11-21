#' @export
`getMatrix` <- function(noflevels, depth = 0) {
    # old version of the createMatrix() code in package QCA
    
    nofconds <- length(noflevels)
    pwr <- unique(noflevels)
    
    limited <- depth > 0 && depth < sum(noflevels > 1)
    if (limited && all(noflevels >= 1)) {
        # Count suffixes by the number of nonzero positions still allowed.
        budget <- floor(depth)
        counts <- matrix(1, nofconds + 1L, budget + 1L)
        for (j in rev(seq_len(nofconds))) {
            counts[j, -1L] <- counts[j + 1L, -1L] +
                (noflevels[j] - 1) * counts[j + 1L, -(budget + 1L)]
        }
        nrows <- counts[1L, budget + 1L]
        if (nrows > .Machine$integer.max) stopError("Too many configurations for an R matrix.")
        result <- matrix(0, nrows, nofconds)
        rank <- seq_len(nrow(result)) - 1
        left <- rep(budget, length(rank))
        for (j in seq_len(nofconds)) {
            zero <- counts[j + 1L, left + 1L]
            use <- which(rank >= zero)
            if (length(use)) {
                block <- counts[j + 1L, left[use]]
                offset <- rank[use] - zero[use]
                result[use, j] <- 1 + offset %/% block
                rank[use] <- offset %% block
                left[use] <- left[use] - 1L
            }
        }
        return(result)
    }

    # rep.int comes from rep internal, not rep integers
    
    if (length(pwr) == 1) {
        create <- function(idx) {
            rep.int(c(sapply(seq_len(pwr) - 1, function(x) rep.int(x, pwr^(idx - 1)))),
                    pwr^nofconds/pwr^idx)
        }
        retmat <- sapply(rev(seq_len(nofconds)), create)
    } else {
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        orep  <- cumprod(rev(c(rev(noflevels)[-1], 1)))
        retmat <- sapply(seq_len(nofconds), function(x) {
           rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x])
        })
    }
    
    if (is.vector(retmat)) {
        retmat <- matrix(retmat, nrow = 1)
    }

    if (limited) {
        retmat <- retmat[apply(retmat, 1, function(x) sum(x > 0) <= depth ), , drop = FALSE]
    }
    
    return(retmat)
}

