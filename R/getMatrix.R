`getMatrix` <- function(noflevels, depth = 0) {
    # old version of the createMatrix() code in package QCA
    
    nofconds <- length(noflevels)
    pwr <- unique(noflevels)
    
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

    if (depth > 0) {
        retmat <- retmat[apply(retmat, 1, function(x) sum(x > 0) <= depth ), , drop = FALSE]
    }
    
    return(retmat)
}

