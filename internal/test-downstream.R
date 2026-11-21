# Run from the admisc project root. Each invocation tests one installed build.
run_release_checks <- function(stages = c("admisc", "declared", "QCA", "benchmark"),
                               declared_path = "../declared", qca_path = "../QCA") {
    if (!file.exists("DESCRIPTION") || read.dcf("DESCRIPTION", "Package")[1L] != "admisc") {
        stop("Run the release checks from the admisc project root.")
    }
    stages <- match.arg(stages, c("admisc", "declared", "QCA", "benchmark"), several.ok = TRUE)
    checkouts <- c(admisc = ".", declared = declared_path, QCA = qca_path, benchmark = ".")
    full_release <- setequal(stages, names(checkouts))
    suites <- list(admisc = "tests/testthat", declared = "tests/testthat",
                   QCA = c("tests/testthat", "internal/tests/testthat"))
    for (stage in intersect(stages, names(suites))) {
        checkout <- checkouts[[stage]]
        desc <- file.path(checkout, "DESCRIPTION")
        if (!file.exists(desc) || read.dcf(desc, "Package")[1L] != stage) {
            stop("Supply the path to a ", stage, " source checkout.")
        }
        for (suite in suites[[stage]]) {
            if (!length(list.files(file.path(checkout, suite), pattern = "^test-.*[.][rR]$"))) {
                stop("Missing tests in ", file.path(checkout, suite), ".")
            }
        }
    }

    work <- tempfile("admisc-release-")
    lib <- file.path(work, "library")
    dir.create(lib, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE), add = TRUE)
    tarball <- pkgbuild::build(".", dest_path = work, vignettes = FALSE)
    results <- list()
    if (full_release) {
        sha256 <- digest::digest(file = tarball, algo = "sha256")
        callr::rcmd("check", c("--as-cran", tarball), wd = work, show = TRUE, fail_on_status = TRUE,
                    env = c(callr::rcmd_safe_env(), LANGUAGE = "en"))
        log <- readLines(file.path(work, "admisc.Rcheck", "00check.log"), warn = FALSE)
        status <- grep("^Status:", log, value = TRUE)
        if (!identical(status, "Status: OK")) {
            stop("Release requires R CMD check --as-cran to finish with Status: OK. ", paste(status, collapse = " "))
        }
        results$check <- status
    }
    callr::rcmd("INSTALL", c(paste0("--library=", lib), tarball),
                show = TRUE, fail_on_status = TRUE)

    for (stage in stages) {
        args <- list(stage = stage, checkout = normalizePath(checkouts[[stage]]),
                     suites = suites[[stage]], lib = lib,
                     stress_results = getOption("admisc.stress_results"))
        result <- callr::r(run_release_stage, args = args, libpath = c(lib, .libPaths()),
                           user_profile = FALSE, show = TRUE)
        results[[stage]] <- result
        if (stage != "benchmark") {
            files <- if ("suite" %in% names(result)) file.path(result$suite, result$file) else result$file
            message(sprintf("%s: %d test blocks in %d files, %d expectations passed, %d warnings, %d skipped blocks.",
                            stage, nrow(result), length(unique(files)), sum(result$passed),
                            sum(result$warning), sum(result$skipped)))
            if (full_release && any(result$failed | result$error | result$warning | result$skipped)) {
                stop("Release requires all ", stage, " tests to pass without warnings or skips.")
            }
        }
    }
    if (full_release) {
        results$tarball <- keep_release_tarball(tarball, sha256)
        results$sha256 <- sha256
        message("Release tarball: ", results$tarball, "\nSHA-256: ", sha256)
    }
    invisible(results)
}

keep_release_tarball <- function(tarball, sha256) {
    directory <- file.path("internal", "releases", sha256)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    destination <- file.path(directory, basename(tarball))
    if (file.exists(destination)) {
        if (!identical(digest::digest(file = destination, algo = "sha256"), sha256)) {
            stop("Checksum mismatch for the previously saved tarball: ", destination)
        }
    } else {
        # Verify the copy on the destination filesystem before publishing its final name.
        pending <- tempfile(".pending-", tmpdir = directory)
        on.exit(unlink(pending), add = TRUE)
        if (!file.copy(tarball, pending) ||
            !identical(digest::digest(file = pending, algo = "sha256"), sha256) ||
            !file.rename(pending, destination)) {
            stop("Could not retain the checked tarball at ", destination)
        }
    }
    normalizePath(destination, mustWork = TRUE)
}

# callr starts this function in a fresh process with the candidate library first.
run_release_stage <- function(stage, checkout, suites, lib, stress_results) {
    path <- getNamespaceInfo(loadNamespace("admisc"), "path")
    stopifnot(identical(normalizePath(path), normalizePath(file.path(lib, "admisc"))))
    message("Running ", stage, " with admisc ", packageVersion("admisc"), " from ", path)
    setwd(checkout)
    Sys.setenv(NOT_CRAN = "true")

    if (stage == "declared") {
        Sys.unsetenv("DECLARED_CHECK_ADMISC_SYNC")
        return(as.data.frame(testthat::test_local(checkout, reporter = "summary", stop_on_failure = TRUE)))
    }

    if (stage == "QCA") {
        tarball <- pkgbuild::build(checkout, dest_path = dirname(lib), vignettes = FALSE)
        callr::rcmd("INSTALL", c(paste0("--library=", lib), tarball),
                    libpath = .libPaths(), show = TRUE, fail_on_status = TRUE)
        library(QCA, lib.loc = lib)
        stopifnot(identical(normalizePath(getNamespaceInfo(asNamespace("QCA"), "path")),
                            normalizePath(file.path(lib, "QCA"))))
    } else {
        library(admisc, lib.loc = lib)
        if (stage == "benchmark") {
            source("internal/benchmarks/grouped-mean.R", local = TRUE)
            return(benchmark_grouped_mean())
        }
        options(admisc.stress_results = stress_results)
    }

    runs <- lapply(suites, function(suite) {
        message("Testing ", stage, " ", packageVersion(stage), " ", suite)
        tests <- as.data.frame(testthat::test_dir(suite, reporter = "summary", stop_on_failure = TRUE))
        tests$suite <- suite
        tests
    })
    do.call(rbind, runs)
}

test_declared <- function(declared_path = "../declared") {
    invisible(run_release_checks("declared", declared_path = declared_path)$declared)
}

test_qca <- function(qca_path = "../QCA") {
    invisible(run_release_checks("QCA", qca_path = qca_path)$QCA)
}
