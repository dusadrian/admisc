`getLevels` <- 
function(data) {
    data <- as.data.frame(data)
    colnames <- paste("V", ncol(data), sep = ".")
    pN <- unlist(lapply(data, possibleNumeric))
    noflevels <- rep(NA, ncol(data))
    noflevels[pN] <- apply(data[, pN, drop = FALSE], 2, max) + 1
    noflevels[pN][noflevels[pN] == 1] <- 2
    noflevels[pN][apply(data[, pN, drop = FALSE], 2, function(x) any(x %% 1 > 0))] <- 2
    return(as.vector(noflevels))
}
