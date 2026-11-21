test_that("addCases preserves rows, column selection, and types", {
    x <- data.frame(id = 1:2, age = c(25, 40))
    y <- data.frame(score = c(8, 9), id = 3:4)
    originalx <- x
    originaly <- y

    common <- addCases(x, y)
    all <- addCases(x, y, keep = "all")
    first <- addCases(x, y, keep = "x")
    second <- addCases(x, y, keep = "y")

    expect_identical(names(common), "id")
    expect_identical(names(all), c("id", "age", "score"))
    expect_identical(names(first), names(x))
    expect_identical(names(second), names(y))
    expect_identical(all$id, 1:4)
    expect_identical(all$age, c(25, 40, NA_real_, NA_real_))
    expect_identical(all$score, c(NA_real_, NA_real_, 8, 9))
    expect_identical(x, originalx)
    expect_identical(y, originaly)
    expect_identical(addCases(x, y, "all", " age, score "), common)
    expect_identical(addCases(x, y, "all", c("age", "score")), common)
    expect_identical(addCases(x, y, "all", ""), all)

    # The two selections used in the Add cases dialog examples.
    mixed <- addCases(PlantGrowth, ToothGrowth, "all", "group, dose")
    right <- addCases(PlantGrowth, ToothGrowth, "y")
    expect_identical(names(mixed), c("weight", "len", "supp"))
    expect_identical(names(right), names(ToothGrowth))
    expect_true(nrow(mixed) == nrow(PlantGrowth) + nrow(ToothGrowth))
    expect_true(all(is.na(right$len[seq_len(nrow(PlantGrowth))])))
    expect_true(is.factor(mixed$supp))
    expect_identical(levels(mixed$supp), levels(ToothGrowth$supp))
    expect_identical(tail(right$len, nrow(ToothGrowth)), ToothGrowth$len)

    # Disjoint inputs require an explicit choice; character columns stay character.
    # Unequal row counts also check that missing row indices are not recycled.
    left <- data.frame(a = 1L)
    right <- data.frame(b = c("red", "green", "blue"))
    joined <- addCases(left, right, "all")
    commonError <- tryCatch(addCases(left, right), error = conditionMessage)
    expect_true(grepl("no common variables", commonError))
    expect_identical(dim(joined), c(4L, 2L))
    expect_identical(joined$a, c(1L, NA_integer_, NA_integer_, NA_integer_))
    expect_identical(joined$b, c(NA_character_, right$b))
    expect_identical(addCases(right, left, "all")$b, c(right$b, NA_character_))
    expect_identical(addCases(left, right, "x"), joined["a"])
    expect_identical(addCases(left, right, "y"), joined["b"])

    # Missing columns retain their classes in either direction, including dates.
    dated <- data.frame(id = 3L, date = as.Date("2026-01-01"))
    dated$time <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
    dated$group <- ordered("b", levels = c("a", "b"))
    for (result in list(addCases(x, dated, "all"), addCases(dated, x, "all"))) {
        expect_true(inherits(result$date, "Date"))
        expect_true(inherits(result$time, "POSIXct"))
        expect_identical(attr(result$time, "tzone"), "UTC")
        expect_true(is.ordered(result$group))
        expect_identical(levels(result$group), c("a", "b"))
        expect_true(sum(is.na(result$date)) == nrow(x))
    }

    # The retained schema and row count survive empty inputs and a zero-column input.
    emptyx <- x[FALSE, , drop = FALSE]
    emptyy <- y[FALSE, , drop = FALSE]
    expect_true(nrow(addCases(emptyx, y, "all")) == nrow(y))
    expect_true(nrow(addCases(x, emptyy, "all")) == nrow(x))
    expect_true(nrow(addCases(emptyx, emptyy, "all")) == 0L)
    expect_identical(names(addCases(emptyx, emptyy, "all")), names(all))
    expect_true(nrow(addCases(x[, FALSE, drop = FALSE], y, "all")) == nrow(all))

    # Shared factors use the union of levels and shared numeric types follow rbind().
    f1 <- data.frame(id = 1L, group = factor("a"))
    f2 <- data.frame(group = factor("b"), id = 2.5)
    factors <- addCases(f1, f2)
    expect_identical(factors$id, c(1, 2.5))
    expect_identical(levels(factors$group), c("a", "b"))
    expect_identical(as.character(factors$group), c("a", "b"))

    # Names are matched exactly, including spaces and literal commas.
    names(x) <- c("case id", "age, years")
    names(y) <- c("score value", "case id")
    expect_identical(names(addCases(x, y, "all", "age, years")), c("case id", "score value"))
    expect_identical(addCases(x, y, "all", c("age, years", "score value")), addCases(x, y))

    fails <- function(expr) {
        inherits(tryCatch(expr, error = identity), "error")
    }
    expect_true(fails(addCases(1:3, y)))
    expect_true(fails(addCases(x, y, keep = "invalid")))
    expect_true(fails(addCases(x, y, exclude = NA_character_)))
    expect_true(fails(addCases(x, y, exclude = 1)))
    expect_true(fails(addCases(x, y, exclude = "missing")))
    expect_true(fails(addCases(x, y, exclude = "case id")))
    expect_true(fails(addCases(PlantGrowth, ToothGrowth)))
    expect_true(fails(addCases(setNames(x, c("a", "a")), y)))
    expect_true(fails(addCases(setNames(x, c("", "b")), y)))

    # Large schemas need no explicit variable list at the call site.
    wide1 <- as.data.frame(matrix(seq_len(4000), nrow = 2))
    wide2 <- as.data.frame(matrix(seq_len(4000), nrow = 2))
    names(wide1) <- paste0("v", seq_len(2000))
    names(wide2) <- paste0("v", 1001:3000)
    wide <- addCases(wide1, wide2, keep = "all")
    expect_identical(dim(wide), c(4L, 3000L))
    expect_identical(names(wide), paste0("v", seq_len(3000)))
    expect_true(all(is.na(wide$v1[3:4])))
    expect_true(all(is.na(wide$v3000[1:2])))

    # Explicit additions retain source order and work without shared columns.
    x <- data.frame(A = 1:2, B = c("a", "b"), C = 3:4)
    y <- data.frame(C = 5:7, D = c("d", "e", "f"), E = 8:10)
    expect_identical(names(addCases(x, y, add.x = "A")), c("A", "C"))
    expect_identical(names(addCases(x, y, add.y = "D")), c("C", "D"))
    expect_identical(names(addCases(x, y, add.x = "A", add.y = "D")), c("A", "C", "D"))
    expect_identical(addCases(x, y, add.x = "A, B"), addCases(x, y, keep = "x"))
    expect_identical(addCases(x, y, add.x = "B", exclude = "B"), addCases(x, y))
    expect_identical(addCases(x, y, add.y = "D")$D, c(NA_character_, NA_character_, y$D))
    expect_identical(names(addCases(x["A"], y["D"], add.y = "D")), "D")
    expect_true(fails(addCases(x, y, add.x = "D")))
    expect_true(fails(addCases(x, y, add.y = "A")))
    expect_true(fails(addCases(x, y, add.x = NA_character_)))
    # Sparse selections in a large schema need only the added names.
    expect_true(ncol(addCases(wide1, wide2, add.x = "v1", add.y = "v3000")) == 1002L)
})
