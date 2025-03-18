testthat::test_that(
  "ensure_packages correctly handles all packages already installed", {
    mockery::stub(ensure_packages, "cli::cli_alert_success", function(...) NULL)

    installed_pkgs <- c("stats", "utils")
    testthat::expect_invisible(ensure_packages(installed_pkgs))
  })

testthat::test_that(
  "ensure_packages installs missing packages interactively when user agrees", {
    mockery::stub(ensure_packages, "interactive", function() TRUE)
    mockery::stub(ensure_packages, "readline", function(...) "y")
    mockery::stub(ensure_packages, "utils::install.packages",
                  function(pkg, ...) NULL)
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_success", function(...) NULL)

    missing_pkgs <- c("crayon")
    testthat::expect_invisible(ensure_packages(missing_pkgs))
  })

testthat::test_that(
  "ensure_packages skips installation interactively when user declines", {
    mockery::stub(ensure_packages, "interactive", function() TRUE)
    mockery::stub(ensure_packages, "readline", function(...) "n")
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_warning", function(...) NULL)

    missing_pkgs <- c("pillar")
    testthat::expect_invisible(ensure_packages(missing_pkgs))
  })

testthat::test_that(
  "ensure_packages skips installation in non-interactive sessions", {
    mockery::stub(ensure_packages, "interactive", function() FALSE)
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_warning", function(...) NULL)

    missing_pkgs <- c("glue")
    testthat::expect_invisible(ensure_packages(missing_pkgs))
  })

testthat::test_that(
  "ensure_packages handles invalid package names gracefully", {
    mockery::stub(ensure_packages, "interactive", function() TRUE)
    mockery::stub(ensure_packages, "readline", function(...) "y")
    mockery::stub(ensure_packages, "utils::install.packages",
                  function(pkg, ...) stop("package not found"))
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_danger", function(...) NULL)

    invalid_pkgs <- c("thispackagedoesnotexist123")
    testthat::expect_invisible(ensure_packages(invalid_pkgs))
  })
