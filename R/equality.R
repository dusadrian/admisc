`agteb` <- function(a, b) {
    a > b | abs(a - b) <= .Machine$double.eps^0.5
}


`alteb` <- function(a, b) {
    a < b | abs(a - b) <= .Machine$double.eps^0.5
}


`aeqb` <- function(a, b) {
    abs(a - b) <= .Machine$double.eps^0.5
}
