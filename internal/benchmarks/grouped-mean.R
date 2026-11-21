benchmark_grouped_mean <- function(profile = NULL) {
    set.seed(17)
    cat(R.version.string, R.version$platform, "\n")

    timings <- list()
    for (labelled in c(FALSE, TRUE)) {
        n <- 20000L
        d <- data.frame(x = rnorm(n), g = rep(seq_len(50), length.out = n))
        for (j in seq_len(50)) {
            v <- sample(0:4, n, replace = TRUE)
            if (labelled) {
                v <- declared::declared(v, labels = c(no = 0, yes = 1, other = 4), na_values = 4)
            }
            d[[paste0("v", j)]] <- v
        }
        f <- function() using(d, mean(x), split.by = g)
        expected <- vapply(split(d$x, d$g), mean, numeric(1))
        warmup <- f()
        stopifnot(identical(rownames(warmup), names(expected)),
                  identical(as.numeric(warmup), unname(expected)))

        elapsed <- replicate(9L, {
            gc()
            system.time(for (i in seq_len(30L)) f())[["elapsed"]] * 1000 / 30
        })
        kind <- if (labelled) "declared" else "ordinary"
        timings[[kind]] <- data.frame(columns = kind, min_ms = min(elapsed),
                                     median_ms = median(elapsed), max_ms = max(elapsed))
        cat(sprintf("%s unused columns, ms/call   min %.2f   median %.2f   max %.2f\n",
                    kind, min(elapsed), median(elapsed), max(elapsed)))

        if (labelled && !is.null(profile)) {
            gc()
            Rprof(profile, interval = 0.001, event = "elapsed")
            tryCatch(for (i in seq_len(350L)) f(), finally = Rprof(NULL))
            print(head(summaryRprof(profile)$by.total, 30L))
        }
    }
    invisible(do.call(rbind, unname(timings)))
}
