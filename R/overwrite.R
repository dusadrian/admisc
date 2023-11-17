`overwrite` <- function(objname, content, environment) {
    objname <- gsub("'|\"|[[:space:]]", "", objname)
    if (exists(objname, environment)) {
        environment[[objname]] <- content
    }
    else {
        structure_string <- paste(capture.output(dput(content)), collapse = " ")

        eval(
            parse(text = sprintf(paste(objname, "<- %s"), structure_string)),
            envir = environment
        )
    }
}
