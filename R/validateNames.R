`validateNames` <-
function(expression = "", snames = "", data = NULL) {
    if (is.null(data)) {
        ppm <- translate(expression = expression, snames = snames, validate = TRUE)
    }
    else {
        ppm <- translate(expression = expression, data = data, validate = TRUE)
    }
    
    return(ppm[, apply(ppm, 2, function(x) any(x >= 0)), drop = FALSE])
}
