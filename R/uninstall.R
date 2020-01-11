`uninstall` <- function(package) {
    package <- gsub("\\\"", "", deparse(substitute(package)))
    admisc::unload(package)
    if (is.element(package, rownames(installed.packages()))) {
        remove.packages(package)
    }
}
