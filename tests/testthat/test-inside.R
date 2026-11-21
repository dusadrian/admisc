test_that("inside updates each supported input in the caller", {
    local({
        offset <- 5L
        for (x in list(list(a = 1:3), data.frame(a = 1:3))) {
            box <- list(x = x)
            inside(box$x, `new col` = a + offset)
            expect_identical(box$x[["new col"]], 6:8)
        }
    })

    if (requireNamespace("tibble", quietly = TRUE)) {
        x <- tibble::tibble(a = 1:3, b = 3:1, items = as.list(4:6))
        alias <- x
        inside(x, { total <- a + b; a <- 0L; rm(b) })
        expect_identical(x, tibble::tibble(a = rep(0L, 3L), items = as.list(4:6),
                                              total = rep(4L, 3L)))
        expect_identical(names(alias), c("a", "b", "items"))
        x <- tibble::tibble(a = 1:2, j = 3:4, a = 5:6, .name_repair = "minimal")
        inside(x, { a <- a + 1L; rm(j); z <- a })
        expect_identical(names(x), c("a", "a", "z"))
        expect_identical(x[[1L]], 2:3)
        expect_identical(x[[2L]], 5:6)
        expect_identical(x$z, 2:3)

        x <- tibble::as_tibble(setNames(list(1:2, 3:4), c("", "b")), .name_repair = "minimal")
        inside(x, { z <- b; rm(b) })
        expect_identical(names(x), c("", "z"))
        expect_identical(x[[1L]], 1:2)
        expect_identical(x$z, 3:4)

        if (requireNamespace("dplyr", quietly = TRUE)) {
            x <- dplyr::group_by(tibble::tibble(g = c(1L, 1L, 2L), v = 1:3), g)
            inside(x, { avg <- mean(v); g <- 1L })
            expect_true(inherits(x, "grouped_df"))
            expect_identical(x$avg, rep(2, 3L))
            expect_identical(dplyr::group_rows(x)[[1L]], 1:3)
            expect_true(dplyr::n_groups(x) == 1L)
        }
    }

    if (requireNamespace("data.table", quietly = TRUE)) {
        # Enable data.table joins in the package test environment.
        .datatable.aware <- TRUE
        dt <- data.table::data.table(id = 1:3, v = 3:1, spare = 1:3, key = "id")
        data.table::setindexv(dt, "v")
        alias <- dt
        box <- list(table = dt)
        inside(box$table, { result <- id + v; rm(spare) })
        dt <- box$table
        expect_true(data.table::is.data.table(dt))
        expect_identical(dt$result, rep(4L, 3L))
        expect_identical(names(dt), c("id", "v", "result"))
        expect_identical(data.table::key(dt), "id")
        expect_identical(data.table::indices(dt), "v")
        inside(dt, id = -id)
        expect_true(is.null(data.table::key(dt)))
        expect_identical(data.table::indices(dt), "v")
        joined <- dt[list(-1L), on = "id"]
        expect_identical(joined$v, 3L)
        inside(dt, v <- -v)
        expect_true(!"v" %in% data.table::indices(dt))
        joined <- dt[list(-1L), on = "v"]
        expect_identical(joined$id, -3L)
        expect_identical(alias$id, 1:3)
        expect_identical(alias$v, 3:1)
        expect_identical(names(alias), c("id", "v", "spare"))
        before <- data.table::copy(dt)
        failed <- try(inside(dt, { id <- 9L; bad <- 1:2 }), silent = TRUE)
        expect_true(inherits(failed, "try-error"))
        expect_identical(dt, before)
    }

    local({
        m <- cbind(x = 1:3, `two words` = 4:6)
        dimnames(m) <- list(case = c("a", "a", "c"), variable = colnames(m))
        original <- m
        offset <- 2L
        inside(m, { total <- x + `two words` + offset; x <- 9L; rm(`two words`) })
        expected <- cbind(x = rep(9L, 3L), total = c(7L, 9L, 11L))
        dimnames(expected) <- list(case = rownames(original), variable = colnames(expected))
        expect_identical(m, expected)
        expect_identical(unname(original[, 1L]), 1:3)

        box <- list(m = m)
        inside(box$m, label = "yes")
        expect_true(is.character(box$m))
        expect_identical(unname(box$m[, "x"]), rep("9", 3L))
        expect_true(all(box$m[, "label"] == "yes"))

        inside(m, total <- NULL)
        expect_identical(m, expected[, "x", drop = FALSE])
        for (bad in list(1:2, 1:4, matrix(1:3))) {
            failed <- try(inside(m, { x <- 0L; z <- bad }), silent = TRUE)
            expect_true(inherits(failed, "try-error"))
            expect_identical(m, expected[, "x", drop = FALSE])
        }
        inside(m, rm(x))
        expect_identical(m, expected[, FALSE, drop = FALSE])
        inside(m, x = 9L)
        expect_identical(m, expected[, "x", drop = FALSE])

        m <- original[FALSE, , drop = FALSE]
        inside(m, z = 1L)
        expect_identical(dim(m), c(0L, 3L))
        expect_true(is.integer(m))
        m <- unname(original)
        failed <- try(inside(m, y = 1), silent = TRUE)
        expect_true(inherits(failed, "try-error"))
        expect_identical(m, unname(original))
    })
})

test_that("inside changes bindings directly in the supplied environment", {
    parent <- list2env(list(x = 2L), parent = baseenv())
    e <- new.env(parent = parent)
    alias <- e
    x <- 100L
    result <- withVisible(inside(e, x = x + 1L))
    expect_identical(result$value, alias)
    expect_false(result$visible)
    expect_identical(alias$x, 3L)
    expect_identical(parent$x, 2L)
    expect_identical(x, 100L)

    get_env <- function() e
    inside(get_env(), { x <- x + 1L; .saved <- x; nullable <- NULL; rm(x) })
    expect_false(exists("x", alias, inherits = FALSE))
    expect_identical(alias$.saved, 4L)
    expect_true(exists("nullable", alias, inherits = FALSE))
    expect_null(alias$nullable)

    lockBinding(".saved", e)
    expect_error(inside(e, { before <- TRUE; .saved <- 5L; after <- TRUE }))
    expect_identical(alias$.saved, 4L)
    expect_true(alias$before)
    expect_false(exists("after", alias, inherits = FALSE))
})

test_that("inside edits named atomic vectors without dropping their type or names", {
    for (values in list(c(TRUE, FALSE), 1:2, c(1, 2), c(1i, 2i), c("x", "y"), as.raw(1:2))) {
        original <- setNames(values, c("a", "b"))
        box <- list(v = original)
        result <- withVisible(inside(box$v, { added <- a; a <- b; rm(b) }))
        expect_identical(box$v, setNames(rev(values), c("a", "added")))
        expect_identical(result$value, box$v)
        expect_false(result$visible)
        inside(box$v, { a <- NULL; rm(added) })
        expect_identical(box$v, original[FALSE])
        inside(box$v, restored = original[[1L]])
        expect_identical(box$v, setNames(values[1L], "restored"))
    }

    v <- c(a = 1L, b = 2L)
    a <- 100L
    offset <- 0.5
    inside(v, `new name` = a + offset)
    expect_identical(v, c(a = 1, b = 2, `new name` = 1.5))
    inside(v, b <- "two")
    expect_identical(v, c(a = "1", b = "two", `new name` = "1.5"))

    f <- setNames(factor(c("low", "high"), levels = c("low", "high")), c("a", "b"))
    inside(f, { a <- "high"; extra <- b; rm(b) })
    expect_identical(f, setNames(factor(c("high", "high"), levels = c("low", "high")), c("a", "extra")))
    d <- setNames(as.Date("2020-01-01"), "start")
    inside(d, finish = start + 1)
    expect_identical(d, setNames(as.Date(c("2020-01-01", "2020-01-02")), c("start", "finish")))

    v <- setNames(1:5, c("a", "a", "", NA_character_, ".hidden"))
    inside(v, { a <- 7L; copy <- .hidden; rm(.hidden) })
    expect_identical(v, setNames(c(7L, 2:5), c("a", "a", "", NA_character_, "copy")))
    v <- c(a = 1L, middle = 2L, a = 3L)
    inside(v, rm(a))
    expect_identical(v, c(middle = 2L, a = 3L))
})

test_that("inside rejects invalid vector assignments before writing back", {
    original <- c(a = 1L, b = 2L)
    v <- original
    for (bad in list(1:2, integer(), list(1L), matrix(1L), new.env())) {
        expect_error(inside(v, { a <- 3L; z <- bad }), "atomic scalar")
        expect_identical(v, original)
    }
    v <- unname(v)
    expect_error(inside(v, x = 1L), "named atomic vector")
})
