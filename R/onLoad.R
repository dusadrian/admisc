.onLoad <- function(libname, pkgname) {
  options(admisc.tol = .Machine$double.eps^0.5)
}

.onUnload <- function(libname, pkgname) {
  options(admisc.tol = NULL)
}
