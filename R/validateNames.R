`validateNames` <-
function(expression = "", snames = "", data = NULL) {

    # pof("SR~V + CLP~V + SCRP + SLRP -> ~JSR", data = d.jobsecurity)
    # here, there are no * signs in the expression, and d.jobsecurity
    # contains both single letter conditions and multi-letter outcome

    # I could have used all.vars(parse(text = expression)) but with
    # single letter condirions this does not work
    
    if (is.null(data)) {
        ppm <- translate(expression = expression, snames = snames, validate = TRUE)
    }
    else {
        ppm <- translate(expression = expression, data = data, validate = TRUE)
    }
    
    return(ppm[, apply(ppm, 2, function(x) any(x >= 0)), drop = FALSE])
}
