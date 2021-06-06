`agtb` <- function(a, b) {
    tol <- getOption("admisc.tol")
    (a - tol) > b
}

`altb` <- function(a, b) {
    tol <- getOption("admisc.tol")
    a < (b - tol)
}

`agteb` <- function(a, b) {
    tol <- getOption("admisc.tol")
    (a + tol) > b
}

`alteb` <- function(a, b) {
    tol <- getOption("admisc.tol")
    a < (b + tol)
}

`aeqb` <- function(a, b) {
    tol <- getOption("admisc.tol")
    abs(a - b) < tol
}

`aneqb` <- function(a, b) {
    tol <- getOption("admisc.tol")
    abs(a - b) > tol
}
