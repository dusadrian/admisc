`hclr` <-
function(x, starth = 25, c = 50, l = 75, alpha = 1, fixup = TRUE) {
    if (length(x) > 1) {
        x <- length(table(x))
    }
    return(
        hcl(
            h = seq(starth, starth + 360, length = x + 1)%%360,
            c = c,
            l = l,
            alpha = alpha,
            fixup = fixup
        )[1:x]
    )
}
