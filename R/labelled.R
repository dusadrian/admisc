

`unmix_labelled` <- function(x) {
    # `unmx` <- function(x) {
    attrx <- attributes(x)
    attributes(x) <- NULL

    tagged <- has_tag(x)

    tagged_values <- attrx[["tagged_values"]]
    nms <- names(tagged_values)
    
    if (any(tagged)) {
        tags <- lapply(as.list(get_tag(x[tagged])), function(x) {
            if (!is.na(suppressWarnings(as.numeric(x)))) {
                x <- as.numeric(x)
            }
            return(x)
        })
        

        numtags <- unlist(lapply(tags, is.numeric))
        if (sum(numtags) > 0) {
            x[which(tagged)[numtags]] <- unlist(tags[numtags])
            tagged[which(tagged)[numtags]] <- FALSE
            tags <- unlist(tags[!numtags])
        }
        
        if (!is.null(tags) && !is.null(tagged_values)) {
            x[which(tagged)[is.element(tags, nms)]] <- unname(tagged_values[match(tags[is.element(tags, nms)], nms)])
            tagged[which(tagged)[is.element(tags, nms)]] <- FALSE
        }
    }

    if (sum(tagged) > 0) {
        cat("\n")
        stop("There should not be undeclared missing values into a mixed labelled object.\n\n", call. = FALSE)
    }

    # ------------------
    # the unclass part is VERY important to stay here, BEFORE replacing the values in the labels
    # because of my own choice of automatically transforming any (real) value into a tagged NA
    # when adding (but the same happens when replacing) values into a mixed_labelled object
    labels <- unclass(attrx$labels)
    
    # ------------------
    if (!is.null(labels)) {
        tagged <- has_tag(labels)
        
        if (any(tagged)) {
            tags <- lapply(as.list(get_tag(labels[tagged])), function(x) {
                if (!is.na(suppressWarnings(as.numeric(x)))) {
                    x <- as.numeric(x)
                }
                return(x)
            })
            
            numtags <- unlist(lapply(tags, is.numeric))
            
            if (sum(numtags) > 0) {
                labels[which(tagged)[numtags]] <- unlist(tags[numtags])
                tagged[numtags] <- FALSE
                tags <- unlist(tags[!numtags])
            }

            if (!is.null(tags) && !is.null(tagged_values)) {
                labels[which(tagged)[is.element(tags, nms)]] <- unname(tagged_values[match(tags[is.element(tags, nms)], nms)])
            }

        }

        attr(labels, "tagged_values") <- NULL # just in case
        attrx$labels <- labels
    }

    attrx$tagged_values <- NULL
    attrx$class <- c("haven_labelled_spss", setdiff(attrx$class, "mixed_labelled"))
    attributes(x) <- attrx
    return(x)
}



`order_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE,
    user_na = c("last", "first", "ignore", "na"), na_value = c("last", "first", "na")) {

    according_to <- match.arg(according_to)
    user_na <- match.arg(user_na)
    na_value = match.arg(na_value)

    if (inherits(x, "mixed_labelled")) {
        x <- unmix_labelled(x)
    }
    
    labels <- attr(x, "labels", exact = TRUE)
    na_values <- attr(x, "na_values", exact = TRUE)
    na_range <- attr(x, "na_range", exact = TRUE)

    indexes <- seq_along(x)

    attrx <- attributes(x)
    attributes(x) <- NULL

    tagged <- has_tag(x)

    xmis <- isElement(x, na_values)

    if (!is.null(na_range)) {
        xmis <- xmis | (x >= na_range[1] & x <= na_range[2])
    }

    truena <- is.na(x) & !tagged
    na_indexes <- indexes[truena]
    tagged <- tagged[!truena]
    xmis <- xmis[!truena]
    indexes <- indexes[!truena]

    x <- x[!truena]
    
    result <- c()
    if (na_value == "first") {
        result <- na_indexes
        length(na_indexes) <- 0
    }

    y <- x

    ltagged <- has_tag(labels)
    
    if (any(ltagged)) {
        labels <- unclass(labels)
        labels[ltagged] <- get_tag(labels[ltagged])
    }

    if (any(tagged)) {
        y <- unclass(y)
        y[tagged] <- get_tag(x[tagged])
    }

    if (according_to == "labels") {
        haslabels <- is.element(y, labels)
        tagged_no_label <- !haslabels & tagged

        if (any(tagged_no_label)) {
            if (length(result) > 0) {
                result <- c(result, indexes[tagged_no_label])
            }
            else {
                na_indexes <- c(indexes[tagged_no_label], na_indexes)
            }

            indexes <- indexes[!tagged_no_label]
            x <- x[!tagged_no_label]
            y <- y[!tagged_no_label]
            xmis <- xmis[!tagged_no_label]
            tagged <- tagged[!tagged_no_label]
        }
    }

    if (na_value == "na") {
        length(na_indexes) <- 0
    }
    
    z <- y[xmis | tagged]
    if (according_to == "labels") {
        haslabels <- is.element(z, labels)
        z[haslabels] <- names(labels)[match(z[haslabels], labels)]
    }
    if (user_na == "first") {
        result <- c(result, indexes[xmis | tagged][order(z, decreasing = decreasing)])
    }
    else if (user_na == "last") {
        na_indexes <- c(indexes[xmis | tagged][order(z, decreasing = decreasing)], na_indexes)
    }
    
    if (user_na != "ignore") {
        indexes <- indexes[!xmis & !tagged]
        x <- x[!xmis & !tagged]
        y <- y[!xmis & !tagged]
    }
    

    if (according_to == "labels") {
        haslabels <- is.element(y, labels)
        y[haslabels] <- names(labels)[match(y[haslabels], labels)]
        result <- c(result, indexes[order(y, decreasing = decreasing)])
    }
    else {
        result <- c(result, indexes[order(y, decreasing = decreasing)])
    }

    result <- c(result, na_indexes)
    return(result)
}



`sort_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE,
    user_na = c("last", "first", "ignore", "na"), na_value = c("last", "first", "na")) {

    # vec_sort() in package vctrs is somewhat similar, but still does not
    # differentiate between (hence does not sort) different tagged_na values

    return(x[order_labelled(x, according_to = according_to, decreasing = decreasing, user_na = user_na, na_value = na_value)])
}



`names_values` <- function(x) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    if (inherits(x, "mixed_labelled")) {
        x <- unmix_labelled(x)
    }
    
    labels <- attr(x, "labels", exact = TRUE)
    ltagged <- has_tag(labels)

    tagged_labels <- labels[ltagged]
    labels <- labels[!ltagged]
    
    utag <- c()
    tagged <- has_tag(x)

    if (any(tagged)) {
        utag <- sort(unique(get_tag(x[tagged])))
        x <- x[!tagged]
    }

    numtag <- c()
    if (length(utag) > 0) {
        numtag <- tag_na(utag)
        labtag <- c()

        if (length(tagged_labels) > 0) {
            labtag <- get_tag(tagged_labels)
        }

        # names(numtag) <- paste0("NA(", utag, ")")
        names(numtag) <- paste0(".", utag) # TODO
    
        for (i in seq(length(utag))) {
            if (any(isel <- labtag == utag[i])) {
                # only one can be true, impossible more
                names(numtag)[i] <- names(tagged_labels)[isel]
            }
        }
    }

    x <- x[!duplicated(x)]
    xmis <- logical(length(x))

    na_values <- attr(x, "na_values", exact = TRUE)
    na_range <- attr(x, "na_range", exact = TRUE)

    attrx <- attributes(x)
    attributes(x) <- NULL

    if (!is.null(na_values)) {
        xmis <- xmis | is.element(x, na_values)
    }
    
    if (!is.null(na_range)) {
        xmis <- xmis | (x >= na_range[1] & x <= na_range[2])
    }

    # TO DO: sort_labelled()...?!
    
    xnotmis <- sort(x[!xmis])
    xmis <- sort(x[xmis])
    
    if (length(xmis) > 0) {
        names(xmis) <- xmis
        for (i in seq(length(xmis))) {
            if (any(isel <- labels == xmis[i])) {
                names(xmis)[i] <- names(labels)[isel]
            }
        }
    }


    names(xnotmis) <- xnotmis
    if (length(xnotmis) > 0) {
        for (i in seq(length(xnotmis))) {
            if (any(isel <- labels == xnotmis[i])) {
                names(xnotmis)[i] <- names(labels)[isel]
            }
        }
    }

    result <- c(xnotmis, xmis, numtag)
    attr(result, 'missing') <- c(xmis, numtag)

    return(result)
}



`unique_labelled` <- function(x, sort = FALSE, ...) {

    duplicates <- logical(length(x))
    tagged <- has_tag(x)

    ix <- seq_along(x)
    if (any(tagged)) {
        duplicates[ix[tagged][duplicated(get_tag(x[tagged]))]] <- TRUE
    }

    duplicates[ix[!tagged][duplicated(unclass(x[!tagged]))]] <- TRUE

    x <- x[!duplicates]

    oa <- list(...)
    if (is.element("sort", names(oa)) && oa$sort) {
        return(sort_labelled(x, ... = ...))
    }

    return(x)
}



`to_labels` <- function(x) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    if (inherits(x, "mixed_labelled")) {
        x <- unmix_labelled(x)
    }

    tagged <- has_tag(x)
    
    labels <- names_values(x)
    
    attributes(x) <- NULL
    result <- x

    ltagged <- has_tag(labels)

    
    if (any(ltagged)) {
        labels[ltagged] <- get_tag(labels[ltagged])
    }

    if (any(tagged)) {
        result[tagged] <- get_tag(x[tagged])
    }
    
    result[is.element(result, labels)] <- names(labels)[match(result[is.element(result, labels)], labels)]
    
    return(result)
}
