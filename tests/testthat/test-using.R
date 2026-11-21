test_that("grouped evaluation preserves values, scope, and model fits", {
    d <- data.frame(x = c(1, 3, 8, 4, 0, 7), y = c(2, 7, 13, 1, 2, 9),
                    arm = factor(c("b", "a", "a", "b", NA, "b"), levels = c("b", "z", "a")))
    parts <- split(d, d$arm, drop = TRUE)
    x <- 777
    target <- vapply(parts, function(p) mean(p$x) / 7, numeric(1))
    for (input in list(d, as.list(d))) {
        scores <- using(input, mean(get("x")) / 7, split.by = "arm")
        expect_identical(as.numeric(scores), unname(target))
        expect_identical(rownames(scores), names(parts))
        expect_identical(levels(attr(scores, "group.keys")$arm), levels(d$arm))
    }
    expect_true(inherits(try(using(d, mean(x), split_by = arm), silent = TRUE), "try-error"))
    expect_true(inherits(try(using(list(x = 1:5, g = 1:2), sum(x), split.by = g), silent = TRUE), "try-error"))
    expect_identical(using(d[FALSE, ], length(x)), 0L)

    # Standard evaluation, namespaced calls, wrapper forwarding, and data precedence.
    wrapped <- function(dataset, expr, group) using(dataset, expr, split.by = group)
    res <- wrapped(d, base::sum(x), arm)
    expect_identical(as.numeric(res), unname(vapply(parts, function(p) sum(p$x), numeric(1))))
    rebound <- function(v) { v <- 8; using(d, v) }
    expect_identical(rebound(77), 8)
    collision <- d
    collision$.data <- seq_len(nrow(d))
    expect_identical(using(collision, .data[[".data"]]), collision$.data)
    regional <- d
    regional$region <- c("p,q","p","p","p,q","p","p,q")
    regional$zone <- c("r","q,r","q,r","r","q,r","r")
    r <- using(regional, sum(x), split.by = c(region, zone))
    expect_true(!anyDuplicated(rownames(r)))
    expect_true(nrow(attr(r, "group.keys")) == 2L)
    expect_identical(unname(attr(r, "split")[, 1L]), c("p","p,q"))

    # Group results survive arbitrary extraction, including delayed reads in closures.
    gets <- using(d, function() .data$x, split.by = arm)
    expect_identical(gets[[1L]](), parts[[1L]]$x)
    expect_identical(gets[[2L]](), parts[[2L]]$x)
    expect_identical(attr(gets["a"], "split"), "a")
    expect_identical(attr(gets[0], "split"), character())
    nulls <- using(d, if (as.character(arm[1L]) == "a") NULL else 1L, split.by = arm)
    expect_identical(names(nulls), names(parts))
    expect_true(length(nulls) == 2L)
    expect_true(is.null(nulls[[2L]]))
    v <- using(d, sum(x), split.by = arm, .simplify = FALSE)
    expect_true(is.list(v))
    expect_identical(v[[1]], sum(parts[[1]]$x))
    paired <- using(d, if (as.character(arm[1L]) == "a") c(hi=8,lo=3) else c(lo=1,hi=7), split.by=arm)
    expect_identical(paired["a","lo",drop=TRUE], 3)
    expect_identical(rownames(paired["a",]), "a")
    expect_identical(attr(paired[c(2L,1L),], "split"), rev(names(parts)))
    expect_identical(paired[2L], 3)
    tt <- using(d, table(x), split.by = arm)
    expect_true(all(mapply(identical, tt, lapply(parts, function(p) with(p, table(x))))))

    # Formula contains full-sample data in its original environment.
    f <- as.formula("y~x", env = list2env(list(x = d$x, y = d$y)))
    checkFits <- function(fits) {
        expect_true(all(mapply(function(fit, part) {
            nobs(fit) == nrow(part) && identical(coef(fit), coef(lm(y~x, data=part)))
        }, fits, parts)))
    }
    checkFits(using(d, lm(f), split.by = arm))
    checkFits(using(d, stats::lm(y~.), split.by = arm))
    checkFits(using(d, lm(f, data=.data), split.by = arm))
    full <- using(d, lm(f, data=d), split.by=arm)
    expect_true(all(vapply(full, nobs, integer(1)) == nrow(d)))

    seen <- character()
    withCallingHandlers(using(d, { warning("incomplete fit"); 1 }),
        warning = function(w) { seen <<- conditionMessage(w); invokeRestart("muffleWarning") })
    expect_identical(seen, "incomplete fit")
    problem <- structure(list(message="original object", call=NULL), class=c("fit_error","error","condition"))
    caught <- tryCatch(using(d, stop(problem), split.by=arm), fit_error=identity)
    expect_identical(conditionMessage(caught), "original object")
    expect_identical(as.character(attr(caught, "group.keys")$arm), "b")

    # Each escaped mask retains its own rows. Ordinary vectors preserve element
    # names, and matrix and classed columns keep data-frame subsetting semantics.
    wide <- unclass(d)
    wide$x <- setNames(d$x, letters[seq_len(nrow(d))])
    wide$m <- I(cbind(first = d$x, second = d$y))
    wide$day <- as.Date("2020-01-01") + seq_len(nrow(d))
    wide[[".data"]] <- seq_len(nrow(d))
    class(wide) <- "data.frame"
    read_group <- using(wide, function(field) list(get(field), .data[[".data"]]), split.by = arm)
    for (id in rev(names(read_group))) {
        part <- wide[which(wide$arm == id), , drop = FALSE]
        for (field in c("x", "m", "day")) {
            expect_identical(read_group[[id]](field), list(part[[field]], part[[".data"]]))
        }
    }
})

test_that("using evaluates named vector elements in the caller's scope", {
    scores <- c(home = 3L, away = 1L)
    home <- 99L
    bonus <- 5L
    wrapped <- function(data, expr) using(data, expr)
    expect_identical(wrapped(scores, home - away + bonus), 7L)
    expect_identical(using(scores, { home <- home + 1L; home }), 4L)
    result <- withVisible(using(scores, total = home + away))
    expect_identical(result$value, 4L)
    expect_false(result$visible)
    expect_identical(scores, c(home = 3L, away = 1L))
    expect_identical(home, 99L)

    v <- setNames(1:6, c("x", "", NA_character_, "x", "two words", ".data"))
    expect_identical(using(v, c(x, `two words`, .data)), c(1L, 5L, 6L))
    expect_identical(using(v, sort(ls(all.names = TRUE))), sort(c("x", "two words", ".data")))
    expect_identical(using(setNames(integer(), character()), bonus), bonus)
    expect_error(using(1L, bonus), "requires names")
    expect_error(using(scores, home, split.by = away), "split.by")
})

test_that("using exposes each scalar with its extraction type and class", {
    for (v in list(c(TRUE, FALSE), 1:2, c(1, 2), c(1i, 2i), c("x", "y"),
                   as.raw(1:2), factor(c("low", "high")), as.Date("2020-01-01") + 0:1)) {
        names(v) <- c("a", "b")
        expect_identical(using(v, b), v[[2L]])
    }
})
