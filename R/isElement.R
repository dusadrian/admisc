`isElement` <- function(x, y) {
    taggedx <- is_tagged_string(x) | has_tag(x)
    taggedy <- is_tagged_string(y) | has_tag(y)

    if (any(c(taggedx, taggedy))) {
        attributes(x) <- NULL
        attributes(y) <- NULL
        
        if (any(taggedx)) {
            attributes(x) <- NULL
            if (any(has_tag(x))) {
                x[taggedx] <- paste0("__", get_tag(x[taggedx]))
            }
            else {
                x[taggedx] <- paste0("__", get_tag(x[taggedx]))
            }
        }

        if (any(taggedy)) {
            attributes(y) <- NULL
            if (any(has_tag(y))) {
                y[taggedy] <- paste0("__", get_tag(y[taggedy]))
            }
            else {
                y[taggedy] <- paste0("__", get_tag(y[taggedy]))
            }
        }
        
        
    }
    
    return(is.element(x, y))
    
}
