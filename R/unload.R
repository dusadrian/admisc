`unload` <- function(package) {
    package <- gsub("\\\"", "", deparse(substitute(package)))
    
    if (is.element(package, .packages())) { # equivalent of isNamespaceLoaded(package) but better
        detach(paste("package", package, sep = ":"), character.only = TRUE, unload = TRUE)
        unloadNamespace(package)
    }
    
    if (is.element(package, unlist(lapply(library.dynam(), "[[", 1)))) {
        library.dynam.unload(package, libpath = sub("/Meta.*", '', attr(packageDescription(package), "file")))
    }
}
