#' Intersect expressions
#'
#' This function takes two or more SOP expressions (combinations of conjunctions and
#' disjunctions) or even entire minimization objects, and finds their intersection.
#'
#' @name intersection
#' @rdname intersection
#' @rawRd
#' \usage{
#' intersection(..., snames = "", noflevels)
#' }
#'
#' \arguments{
#'   \item{...}{One or more expressions, combined with / or minimization objects
#'   of class \code{"QCA_min"}.}
#'   \item{snames}{A string containing the sets' names, separated by commas.}
#'   \item{noflevels}{Numerical vector containing the number of levels for each set.}
#'   
#' }
#'
#' \details{
#' The initial aim of this function was to provide a software implementation of the
#' intersection examples presented by Ragin (1987: 144-147). That type of example can also
#' be performed with the function \bold{\code{simplify()}}, while this
#' function is now mainly used in conjunction with the \bold{\code{\link[QCA]{modelFit}()}}
#' function from package \bold{\pkg{QCA}}, to assess the intersection between theory and a
#' QCA model.
#'
#' Irrespective of the input type (character expressions and / or minimiation objects),
#' this function is now a wrapper to the main \bold{\code{simplify()}}
#' function (which only accepts character expressions).
#'
#' It can deal with any kind of expressions, but multivalent crisp conditions need additional
#' information about their number of levels, via the argument \bold{\code{noflevels}}.
#'
#' The expressions can be formulated in terms of either lower case - upper case notation
#' for the absence and the presence of the causal condition, or use the tilde notation
#' (see examples below). Usage of either of these is automatically detected, as long as all
#' expressions use the same notation.
#'
#' If the \bold{\code{snames}} argument is provided, the result is sorted according to the order
#' of the causal conditions (set names) in the original dataset, otherwise it sorts the causal
#' conditions in alphabetical order.
#'
#' For minimzation objects of class \code{"QCA_min"}, the number of levels, and the set names are
#' automatically detected.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#'
#' \references{
#' Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
#' Quantitative Strategies}. Berkeley: University of California Press.
#' }
#'
#'
#' \examples{
#' # using minimization objects
#' \dontrun{
#' library(QCA) # if not already loaded
#' ttLF <- truthTable(LF, outcome = "SURV", incl.cut = 0.8)
#' pLF <- minimize(ttLF, include = "?")
#'
#'
#' # for example the intersection between the parsimonious model and
#' # a theoretical expectation
#' intersection(pLF, DEV*STB)
#'
#'
#' # negating the model
#' intersection(negate(pLF), DEV*STB)
#' }
#'
#'
#' # -----
#' # in Ragin's (1987) book, the equation E = SG + LW is the result
#' # of the Boolean minimization for the ethnic political mobilization.
#'
#' # intersecting the reactive ethnicity perspective (R = lw)
#' # with the equation E (page 144)
#' intersection(~L~W, SG + LW, snames = c(S, L, W, G))
#'
#'
#' # resources for size and wealth (C = SW) with E (page 145)
#' intersection(SW, SG + LW, snames = c(S, L, W, G))
#'
#'
#' # and factorized
#' factorize(intersection(SW, SG + LW, snames = c(S, L, W, G)))
#'
#'
#' # developmental perspective (D = L~G) and E (page 146)
#' intersection(L~G, SG + LW, snames = c(S, L, W, G))
#'
#'
#' # subnations that exhibit ethic political mobilization (E) but were
#' # not hypothesized by any of the three theories (page 147)
#' # ~H = ~(~L~W + SW + L~G)
#' intersection(negate(~L~W + SW + L~G), SG + LW, snames = c(S, L, W, G))
#' }
#'
#' \keyword{functions}
NULL
#' @export
`intersection` <- function(..., snames = "", noflevels = NULL) {
    
    # (function(...)substitute(...()))(x = A, y = B)
    # $x
    # A

    # $y
    # B

    # (function(...)substitute(list(...)))(x = A, y = B)
    # list(x = A, y = B)

    # is.list(aa)
    # [1] TRUE
    # names(aa)
    # [1] "x" "y"
    # is.list(bb)
    # [1] FALSE
    # bb[[2]] # si totusi se comporta ca o lista
    # A
    # names(bb)
    # [1] ""  "x" "y"
    
    dots <- substitute(list(...))
    
    if (length(dots) > 1) {
        # lapply(dots, recreate) messes up with parent.frame(), I think
        # because it introduces an yet another parent before recreate()
        for (i in seq(2, length(dots))) {
            dots[[i]] <- recreate(dots[[i]])
        }
    }
    
    dots <- eval(dots)
    
    snames <- recreate(substitute(snames))
    
    if (length(dots) == 0) {
        stopError("Nothing to intersect.")
    }

    ### probably unnecessary hack to allow package admisc being checked without package QCA
    # e.g. via negate()
    if (length(dots[[1]]) == 0) {
        return(invisible(character(0)))
    }
    ###
    
    snames <- splitstr(snames)
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))
    
    
    isol <- NULL
    
    for (i in seq(length(dots))) {
        x <- dots[[i]]

        if (methods::is(dots[[i]], "QCA_min")) {
            
            if (identical(snames, "")) {
                snames <- dots[[i]]$tt$options$conditions
                if (dots[[i]]$options$use.letters) {
                    snames <- LETTERS[seq(length(snames))]
                }
            }
            
            if (is.element("i.sol", names(x))) {
                elengths <- unlist(lapply(dots[[i]]$i.sol, function(x) length(x$solution)))
                isol <- paste(rep(names(dots[[i]]$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
                
                dots[[i]] <- as.vector(unlist(lapply(dots[[i]]$i.sol, function(x) {
                    lapply(x$solution, paste, collapse = " + ")
                })))
            }
            else {
                dots[[i]] <- as.vector(unlist(lapply(dots[[i]]$solution, paste, collapse = " + ")))
            }
        }
        else if (methods::is(dots[[i]], "admisc_deMorgan")) {

            isol <- attr(x, "isol")
            
            dots[[i]] <- unlist(x)

            if (!is.null(attr(x, "snames"))) {
                attr(dots[[i]], "snames") <- attr(x, "snames")
            }
        
            if (!is.null(attr(x, "isol"))) {
                attr(dots[[i]], "isol") <- attr(x, "isol")
            }

            attr(dots[[i]], "minimized") <- attr(x, "minimized")
        }
        
        if (!is.character(dots[[i]])) {
            stopError("Unrecognised input.")
        }
    }
    
    arglist <- list(snames = snames)
    
    if (!is.null(noflevels)) {
        arglist$noflevels <- noflevels
    }
    
    if (requireNamespace("QCA", quietly = TRUE)) {
        combs <- QCA::createMatrix(unlist(lapply(dots, length)))
    } else {
        combs <- getMatrix(unlist(lapply(dots, length)))
    }


    expressions <- result <- character(nrow(combs))
    
    conj <- ifelse(sl, "", "*")
    
    for (i in seq(nrow(combs))) {
        x <- combs[i, ] + 1
        expression <- c()
        for (j in seq(length(x))) {
            expression <- c(expression, dots[[j]][x[j]])
        }
        
        disj <- grepl("[+]", expression)
        if (any(disj)) {
            expression[disj] <- paste("(", expression[disj], ")", sep = "")
        }
        
        if (any(!disj)) {
            ndisj <- which(!disj)
            if (any(ndisj == 1)) {
                expression[1] <- paste(expression[1], conj, sep = "")
            }
            if (any(ndisj == length(expression))) {
                expression[length(expression)] <- paste(conj, expression[length(expression)], sep = "")
            }
            
            if (length(ndisj <- setdiff(ndisj, c(1, length(expression)))) > 0) {
                expression[ndisj] <- paste(conj, expression[ndisj], conj, sep = "")
            }
        }
        
        expressions[i] <- paste(expression, collapse = "")
        # just to make sure this doesn't slip through
        expressions[i] <- gsub("\\*\\(", "(", expressions[i])
        
        result[i] <- do.call(expandBrackets, c(list(expressions[i]), arglist))
    }
    
    if (sl) {
        for (i in seq(length(expressions))) {
            result[i] <- gsub("[*]", "", result[i])
        }
    }
    
    attr(result, "expressions") <- expressions
    
    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }
    
    class(result) <- c("character", "admisc_intersection")
    return(result)
}
