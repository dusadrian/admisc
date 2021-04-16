`isElement` <- function(x, y) {
    taggedx <- tagged_string(x) | tagged_na_value(x)
    taggedy <- tagged_string(y) | tagged_na_value(y)

    if (any(c(taggedx, taggedy))) {
        if (any(taggedx)) {
            attributes(x) <- NULL
            if (any(tagged_na_value(x))) {
                x[taggedx] <- paste0("__", get_na_tag(x[taggedx]))
            }
            else {
                x[taggedx] <- paste0("__", tag_from_string(x[taggedx]))
            }
        }

        if (any(taggedy)) {
            attributes(y) <- NULL
            if (any(tagged_na_value(y))) {
                y[taggedy] <- paste0("__", get_na_tag(y[taggedy]))
            }
            else {
                y[taggedy] <- paste0("__", tag_from_string(y[taggedy]))
            }
        }

        
        attributes(x) <- NULL
        attributes(y) <- NULL
        
        # as.character just to make sure that both are character if only one is tagged
        return(is.element(as.character(x), as.character(y)))
    }
    else {
        return(is.element(x, y))
    }
}
