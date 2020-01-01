`wholeNumeric` <- function(x) {
    all(floor(x) == x, na.rm = TRUE)
}
