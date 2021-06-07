.onLoad <- function(libname, pkgname) {
    options(admisc.tol = .Machine$double.eps^0.5)
}

.onUnload <- function(libpath) {
    options(admisc.tol = NULL)
    library.dynam.unload("admisc", libpath)
}
