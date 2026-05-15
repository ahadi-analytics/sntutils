# tests/testthat/helper-integration.R
# Provides shared helpers for optional integration tests.
# Keeps live network tests out of the normal fast unit-test path.
# RELEVANT FILES: tests/testthat.R,tests/testthat/test-download_chirps.R

skip_if_not_integration <- function(reason = "Integration test skipped") {
  run_integration <- base::tolower(
    base::Sys.getenv("RUN_INTEGRATION_TESTS", unset = "false")
  )

  testthat::skip_if_not(
    run_integration %in% c("1", "true", "yes"),
    base::paste(
      reason,
      "Set RUN_INTEGRATION_TESTS=true to run live service tests."
    )
  )
}
