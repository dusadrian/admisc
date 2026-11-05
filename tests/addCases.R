library(admisc)

x <- data.frame(id = 1:2, age = c(25, 40))
y <- data.frame(score = c(8, 9), id = 3:4)
originalx <- x
originaly <- y

common <- addCases(x, y)
all <- addCases(x, y, keep = "all")
first <- addCases(x, y, keep = "x")
second <- addCases(x, y, keep = "y")

stopifnot(
    identical(names(common), "id"),
    identical(names(all), c("id", "age", "score")),
    identical(names(first), names(x)),
    identical(names(second), names(y)),
    identical(all$id, 1:4),
    identical(all$age, c(25, 40, NA_real_, NA_real_)),
    identical(all$score, c(NA_real_, NA_real_, 8, 9)),
    identical(x, originalx),
    identical(y, originaly),
    identical(addCases(x, y, "all", " age, score "), common),
    identical(addCases(x, y, "all", c("age", "score")), common),
    identical(addCases(x, y, "all", ""), all)
)

# The two selections used in the Add cases dialog examples.
mixed <- addCases(PlantGrowth, ToothGrowth, "all", "group, dose")
right <- addCases(PlantGrowth, ToothGrowth, "y")
stopifnot(
    identical(names(mixed), c("weight", "len", "supp")),
    identical(names(right), names(ToothGrowth)),
    nrow(mixed) == nrow(PlantGrowth) + nrow(ToothGrowth),
    all(is.na(right$len[seq_len(nrow(PlantGrowth))])),
    is.factor(mixed$supp),
    identical(levels(mixed$supp), levels(ToothGrowth$supp)),
    identical(tail(right$len, nrow(ToothGrowth)), ToothGrowth$len)
)

# Disjoint inputs require an explicit choice; character columns stay character.
# Unequal row counts also check that missing row indices are not recycled.
left <- data.frame(a = 1L)
right <- data.frame(b = c("red", "green", "blue"))
joined <- addCases(left, right, "all")
commonError <- tryCatch(addCases(left, right), error = conditionMessage)
stopifnot(
    grepl("no common variables", commonError),
    identical(dim(joined), c(4L, 2L)),
    identical(joined$a, c(1L, NA_integer_, NA_integer_, NA_integer_)),
    identical(joined$b, c(NA_character_, right$b)),
    identical(addCases(right, left, "all")$b, c(right$b, NA_character_)),
    identical(addCases(left, right, "x"), joined["a"]),
    identical(addCases(left, right, "y"), joined["b"])
)

# Missing columns retain their classes in either direction, including dates.
dated <- data.frame(id = 3L, date = as.Date("2026-01-01"))
dated$time <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
dated$group <- ordered("b", levels = c("a", "b"))
for (result in list(addCases(x, dated, "all"), addCases(dated, x, "all"))) {
    stopifnot(
        inherits(result$date, "Date"),
        inherits(result$time, "POSIXct"),
        identical(attr(result$time, "tzone"), "UTC"),
        is.ordered(result$group),
        identical(levels(result$group), c("a", "b")),
        sum(is.na(result$date)) == nrow(x)
    )
}

# The retained schema and row count survive empty inputs and a zero-column input.
emptyx <- x[FALSE, , drop = FALSE]
emptyy <- y[FALSE, , drop = FALSE]
stopifnot(
    nrow(addCases(emptyx, y, "all")) == nrow(y),
    nrow(addCases(x, emptyy, "all")) == nrow(x),
    nrow(addCases(emptyx, emptyy, "all")) == 0L,
    identical(names(addCases(emptyx, emptyy, "all")), names(all)),
    nrow(addCases(x[, FALSE, drop = FALSE], y, "all")) == nrow(all)
)

# Shared factors use the union of levels and shared numeric types follow rbind().
f1 <- data.frame(id = 1L, group = factor("a"))
f2 <- data.frame(group = factor("b"), id = 2.5)
factors <- addCases(f1, f2)
stopifnot(
    identical(factors$id, c(1, 2.5)),
    identical(levels(factors$group), c("a", "b")),
    identical(as.character(factors$group), c("a", "b"))
)

# Names are matched exactly, including spaces and literal commas.
names(x) <- c("case id", "age, years")
names(y) <- c("score value", "case id")
stopifnot(
    identical(names(addCases(x, y, "all", "age, years")), c("case id", "score value")),
    identical(addCases(x, y, "all", c("age, years", "score value")), addCases(x, y))
)

fails <- function(expr) {
    inherits(tryCatch(expr, error = identity), "error")
}
stopifnot(
    fails(addCases(1:3, y)),
    fails(addCases(x, y, keep = "invalid")),
    fails(addCases(x, y, exclude = NA_character_)),
    fails(addCases(x, y, exclude = 1)),
    fails(addCases(x, y, exclude = "missing")),
    fails(addCases(x, y, exclude = "case id")),
    fails(addCases(PlantGrowth, ToothGrowth)),
    fails(addCases(setNames(x, c("a", "a")), y)),
    fails(addCases(setNames(x, c("", "b")), y))
)

# Large schemas need no explicit variable list at the call site.
wide1 <- as.data.frame(matrix(seq_len(4000), nrow = 2))
wide2 <- as.data.frame(matrix(seq_len(4000), nrow = 2))
names(wide1) <- paste0("v", seq_len(2000))
names(wide2) <- paste0("v", 1001:3000)
wide <- addCases(wide1, wide2, keep = "all")
stopifnot(
    identical(dim(wide), c(4L, 3000L)),
    identical(names(wide), paste0("v", seq_len(3000))),
    all(is.na(wide$v1[3:4])),
    all(is.na(wide$v3000[1:2]))
)

# Explicit additions retain source order and work without shared columns.
x <- data.frame(A = 1:2, B = c("a", "b"), C = 3:4)
y <- data.frame(C = 5:7, D = c("d", "e", "f"), E = 8:10)
stopifnot(
    identical(names(addCases(x, y, add.x = "A")), c("A", "C")),
    identical(names(addCases(x, y, add.y = "D")), c("C", "D")),
    identical(names(addCases(x, y, add.x = "A", add.y = "D")), c("A", "C", "D")),
    identical(addCases(x, y, add.x = "A, B"), addCases(x, y, keep = "x")),
    identical(addCases(x, y, add.x = "B", exclude = "B"), addCases(x, y)),
    identical(addCases(x, y, add.y = "D")$D, c(NA_character_, NA_character_, y$D)),
    identical(names(addCases(x["A"], y["D"], add.y = "D")), "D"),
    fails(addCases(x, y, add.x = "D")),
    fails(addCases(x, y, add.y = "A")),
    fails(addCases(x, y, add.x = NA_character_))
)
# Sparse selections in a large schema need only the added names.
stopifnot(ncol(addCases(wide1, wide2, add.x = "v1", add.y = "v3000")) == 1002L)
