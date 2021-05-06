`unlockEnvironment` <- function(env) {
    .Call("_unlockEnvironment", env, PACKAGE = "admisc")
}
