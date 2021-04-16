`unlockEnvironment` <- function(env) {
    .Call("C_unlockEnvironment", env, PACKAGE = "admisc")
}
