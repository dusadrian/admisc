# Local release checks

From the admisc project root, run the full release procedure in the R console.

```r
source("internal/check-release.R")
```

This builds one admisc source tarball and runs `R CMD check --as-cran` on it, including examples and the PDF manual. After `Status: OK`, the command installs that same tarball in a temporary library for the admisc tests, the sibling declared tests, both QCA suites, and the grouped-mean benchmark. Each stage verifies and prints the loaded admisc path.

When all suites pass without warnings or skips and the benchmark completes, the command prints the **tarball to submit to CRAN** and its SHA-256. The file is saved at `internal/releases/<sha256>/admisc_<version>.tar.gz`. The checksum is computed before the check and verified against the saved copy. Different builds of the same version have separate directories. A failed or partial run leaves previous release files in place.

To keep the results in R, use `results <- source("internal/check-release.R")$value`. Use `results$tarball` for the absolute submission path, `results$sha256` for the checksum, and `results$check` for the check status. The entries `admisc`, `declared`, and `QCA` hold the per-test results. The `benchmark` entry holds the minimum, median, and maximum milliseconds per call for each column type.

The default checkouts are `../declared` and `../QCA`. To supply other paths, source `internal/test-downstream.R` and call `run_release_checks(declared_path = "/path/to/declared", qca_path = "/path/to/QCA")`.

## admisc tests

To run only the package tests against the current checkout, use

```r
devtools::test()
```

To run the focused `using()` regressions and its full stress suite against the current checkout, use the filename filter.

```r
devtools::test(filter = "using")
```

The stress suite includes 135 named cases and 750 comparisons against independent R evaluations on seeded data. It uses the locally installed packages `data.table`, `declared`, `haven`, `MASS`, `statistics`, `survival`, and `tibble`. The full package suite also uses `dplyr` for its grouped-tibble check when available.

To test an already installed release candidate, start a fresh R session in the project root, load that installation with `library(admisc)`, and run `testthat::test_dir("tests/testthat", stop_on_failure = TRUE)`. Add `filter = "using"` to select the same subset. The source tarball excludes both `tests/` and `internal/` through `.Rbuildignore`.

## Downstream QCA and declared tests

After the admisc tests, run either downstream check from the same R console.

```r
source("internal/test-downstream.R")
test_qca()
test_declared()
```

Each call builds the current admisc sources, installs the tarball in a temporary library, and verifies the loaded installation path in a fresh R process. A test failure raises an R error. Assign the return value to keep the per-test results.

`test_qca()` builds the sibling `../QCA` checkout against that admisc installation. It runs QCA's package-output tests in `tests/testthat` and its internal regressions in `internal/tests/testthat`, including snapshots and tests marked for local use. Both directories and their fixtures must be present in the QCA checkout. Use `test_qca(qca_path = "/path/to/QCA")` for another location. The returned `suite` column identifies each test's directory.

Selected examples from the archived QCA `tests/testing.R` and `testing.Rout.save` now run directly in admisc. Use `devtools::test(filter = "qca-examples")` to run the translation, fuzzy membership, intersection, factorization, and recoding checks on their own.

`test_declared()` runs the full testthat suite from the sibling `../declared` checkout. Use `test_declared(declared_path = "/path/to/declared")` for another location. One downstream test also reads the admisc source files directly. Its checkout path is set in `declared/tests/testthat/test-admisc-sync.R`.

## Grouped-mean benchmark

Run the benchmark separately in the R console.

```r
devtools::load_all()
source("internal/benchmarks/grouped-mean.R")
benchmark_grouped_mean()
```

It verifies the results against `split()` and `mean()`, then times 20,000 rows, 50 groups, and 50 unused columns. It reports milliseconds per call for ordinary and declared columns across nine batches of 30 calls. Compare the medians on the same machine and R version.

For a profile, call `benchmark_grouped_mean(profile = "grouped-mean.rprof")` with the desired output path. To save the stress suite's `results.tsv` and `random.tsv`, set `options(admisc.stress_results = "/absolute/path/to/existing/directory")` before running the tests. Reset the option with `options(admisc.stress_results = NULL)`.
