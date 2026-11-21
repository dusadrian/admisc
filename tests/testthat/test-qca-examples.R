# Adapted from the archived QCA tests/testing.R and its testing.Rout.save.
# The older examples used lowercase condition names for absence; use explicit ~.
test_that("translation preserves overlapping condition names and multivalue complements", {
    expr <- translate("SUBER + ~SUB*~SET", snames = "SU, BER, SUB, SET")
    expect_identical(colnames(expr), c("SU", "BER", "SUB", "SET"))
    expect_identical(unname(expr["SUBER", ]), c("1", "1", "-1", "-1"))
    expect_identical(unname(expr["~SUB*~SET", ]), c("-1", "-1", "0", "0"))

    expr <- translate("C{1} + ~T{1,2} + T{1}V{0} + C{0}",
                      snames = "C, T, V", noflevels = c(2, 3, 2))
    expect_identical(colnames(expr), c("C", "T", "V"))
    expect_identical(unname(expr["~T{1,2}", ]), c("-1", "0", "-1"))
    expect_identical(unname(expr["T{1}V{0}", ]), c("-1", "1", "0"))
})

test_that("compute reproduces fuzzy memberships for the CVF example", {
    # Cases 1, 2, 5 and 10 exercise both sides of the union.
    data <- data.frame(NATPRIDE = c(.77, .88, .60, .83), GEOCON = c(.95, .35, .35, .05))
    paths <- data.frame(`~NATPRIDE` = c(.23, .12, .40, .17), GEOCON = data$GEOCON,
                        check.names = FALSE)
    expect_equal(compute("~NATPRIDE + GEOCON", data), c(.95, .35, .40, .17))
    expect_equal(compute("~NATPRIDE + GEOCON", data, separate = TRUE), paths)
})

test_that("the mobilization example retains its intersection and factored solution", {
    conditions <- c("S", "L", "W", "G")
    expect_identical(as.character(intersection("~L~W", "SG + LW", snames = conditions)), "S~L~WG")
    result <- intersection("SW", "SG + LW", snames = conditions)
    expect_setequal(strsplit(as.character(result), " + ", fixed = TRUE)[[1L]], c("SWG", "SLW"))
    expect_identical(factorize(result)[[1L]], "SW(G + L)")
})

test_that("recoding handles category ranges with spaces and partial numeric ranges", {
    x <- factor(c("spaces", "that has", "example", "An"), levels = c("An", "example", "that has", "spaces"))
    expect_equal(recode(x, "An : 'that has' = 1; spaces = 2"), c(2, 1, 1, 1))
    expect_equal(recode(x, "An : that has = 1; spaces = 2"), c(2, 1, 1, 1))
    expect_equal(recode(10:20, "8:15=1"), c(rep(1, 6), rep(NA_real_, 5)))
    expect_equal(recode(10:20, "8:15=1; else=copy"), c(rep(1, 6), 16:20))
})
