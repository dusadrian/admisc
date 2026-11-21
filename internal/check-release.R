local({
    source("internal/test-downstream.R", local = TRUE)
    run_release_checks()
})
