test_that("enumeration agrees with independently generated combinations", {
    # Compare both enumeration paths with independently generated ordered results.
    levels <- c(2L, 3L, 1L, 4L)
    full <- getMatrix(levels)
    for (depth in 1:4) {
        expect_identical(getMatrix(levels, depth), full[rowSums(full > 0) <= depth, , drop = FALSE])
    }
    expect_identical(dim(getMatrix(rep(2L, 60), 1)), c(61L, 60L))

    for (n in c(6L, 18L)) {
        for (k in c(1L, 4L, n)) {
            all <- utils::combn(n, k)
            for (threshold in c(1L, n - 1L, n, n + 1L)) {
                selected <- all[, all[k, ] >= threshold, drop = FALSE]
                expect_identical(combnk(n, k, threshold), selected)
                expect_identical(combnk(n, k, threshold, TRUE), selected - 1L)
            }
        }
    }
    expect_identical(dim(combnk(6, 0)), c(0L, 1L))
    expect_identical(dim(combnk(6, 0, 2)), c(0L, 0L))
    expect_identical(combnk(letters[1:6], 3, 5, TRUE), combnk(letters[1:6], 3, 5))
    expect_identical(dim(combnk(100000, 2, 100000)), c(2L, 99999L))
    expect_true(inherits(try(combnk(100000, 2), silent = TRUE), "try-error"))

})

test_that("compute preserves missing values in conjunctions", {
    # NA dominates NaN in a row-wise reduction. Crisp and fuzzy columns also
    # have different missing-value semantics.
    d <- data.frame(A = c(NA, NaN, .2, .8), B = c(NaN, .3, .7, .4),
                    C = c(.3, .4, .1, .2))
    ab <- apply(d[, 1:2], 1L, min)
    expect_identical(compute(A*B+C, data = d), apply(cbind(ab, d$C), 1L, max))
    expect_identical(compute(A*B+C, data = d, separate = TRUE),
                        setNames(data.frame(ab, d$C), c("A*B", "C")))
})

test_that("grouped means retain observed combinations", {
    groups <- data.frame(v = 1:5, g = factor(c("a","b",NA,"a","b"), levels = c("b","c","a")),
                         h = factor(rep("x", 5L)))
    means <- using(groups, mean(v), split.by = c(g,h))
    expect_identical(rownames(means), c("b,x","a,x"))
    expect_identical(as.numeric(means), c(3.5,2.5))
    one <- using(groups[1L, , drop = FALSE], mean(v), split.by = c(g,h))
    expect_identical(rownames(one), "a,x")

})

test_that("text and set operations preserve their output", {
    expect_identical(replaceText("ABC+AB+ABAB", c("A","AB"), c("x","y"), protect="ABC"),
                        "ABC+y+y")
    expect_identical(expandBrackets("(A+B)(C+D)"), "AC + AD + BC + BD")
    expect_identical(factorize("(A+B)(C+D)")[[1L]],
                        c("A(C + D) + B(C + D)", "C(A + B) + D(A + B)"))
    expect_identical(as.character(intersection("AB+C", "AC+B", snames = LETTERS[1:3])),
                        "AB + AC + BC")
})

test_that("recoding and ordering preserve their output", {
    repeated <- rep(c(1,2,3,NA), 30L)
    expect_identical(recode(repeated, "1:2=A;2=B;missing=C"), rep(c("A","B",NA,"C"),30L))
    expect_identical(recode(repeated, cut = c(1,2)), rep(c(1:3,NA_integer_),30L))
    f <- factor(setNames(c("b",NA,"a","c"), letters[1:4]))
    expect_identical(frelevel(f,c("c","a")), stats::relevel(stats::relevel(f,"a"),"c"))
    expect_identical(permutations(c(2L,1L,1L)),
                        matrix(c(2L,1L,1L, 2L,1L,1L, 1L,2L,1L,
                                 1L,1L,2L, 1L,2L,1L, 1L,1L,2L), byrow=TRUE,ncol=3L))
    expect_identical(prettyString(c("AB","CD","EF"), 6, 2), "AB ,\n  CD ,\n  EF")
})

test_that("nested assignment updates the caller", {
    holder <- list(d = data.frame(x = 1:3))
    inside(holder$d, y <- x + 1L)
    expect_identical(holder$d$y, 2:4)
    overwrite("holder$d", quote(x + 1), environment())
    expect_identical(holder$d, quote(x + 1))
})
