`combnk` <- function(n, k, ogte = 0, zerobased = FALSE) {
    
    if (!is.numeric(k)) {
        stopError("Argument k should be numeric.")
    }
    
    if (length(k) != 1L) {
        stopError("Argument k should be a scalar of length 1.")
    }
    
    if (k < 0) {
        stopError("Argument k should be positive.")
    }

    len <- length(n)
    lngt1 <- len > 1

    if (lngt1) {
        if (len < k) {
            stopError("Argument k cannot be greater than the length of n.")
        }
    }
    else {
        if (!is.numeric(n)) {
            stopError("When scalar, argument n should be numeric.")
        }

        if (n < k) {
            stopError("Argument n should be greater than or equal to k.")
        }
    }

    copyn <- n
    if (lngt1) {
        n <- len
    }
    
    if (requireNamespace("QCA", quietly = TRUE)) {
        resmat <- QCA::combint(n = n, k = k, ogte = ogte, zerobased = zerobased)
    }
    else {
        e <- 0L
        ncols <- choose(n, k)
        h <- k - ncols == 1
        
        out <- vector(mode = "list", length = ncols)
        
        comb <- seq.int(k) - zerobased # subtract 1 if zero based
        comb[k] <- comb[k] - 1L
        
        last <- n == k
        i <- 1
        
        while (comb[1] != n - k + 1 || last) {
            last <- FALSE
            if (e < n - h) {
                h <- 1L
                e <- comb[k] + zerobased # add 1 if zero based
                comb[k] <- comb[k] + 1L
                
                if (comb[k] < ogte) {
                    comb[k] <- ogte
                    e <- ogte - 1
                }
            }
            else {
                e <- comb[k - h] + zerobased # add 1 if zero based
                h <- h + 1L
                
                under <- logical(h)
                for (j in seq(h)) {
                    under[j] <- (e + j - zerobased < ogte) # subtract 1 if zero based
                    comb[k - h + j] <- e + j - zerobased  # subtract 1 if zero based
                }
                
                if (all(under)) {
                    comb[k] <- ogte
                    e <- ogte - 1
                    h <- 1L
                }
            }
            
            out[[i]] <- comb
            i <- i + 1
        }
        
        resmat <- do.call("cbind", out[!unlist(lapply(out, is.null))])
    }

    if (lngt1) {
        resmat <- matrix(copyn[resmat], nrow = nrow(resmat))
    }

    return(resmat)
}
