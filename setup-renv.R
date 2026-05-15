# Setup script for renv with sntutils package
# This script handles package installation with fallbacks for problematic packages

# Function to install packages with error handling
install_packages_safely <- function(packages, attempt_source = FALSE) {
  failed_packages <- character(0)
  
  for (pkg in packages) {
    cat("Installing", pkg, "...\n")
    
    # Try binary first, then source if specified
    install_types <- if (attempt_source) c("binary", "source") else "binary"
    
    success <- FALSE
    for (install_type in install_types) {
      tryCatch({
        renv::install(pkg, type = install_type)
        cat("✅", pkg, "installed successfully\n")
        success <- TRUE
        break
      }, error = function(e) {
        cat("❌ Failed to install", pkg, "as", install_type, ":", e$message, "\n")
      })
    }
    
    if (!success) {
      failed_packages <- c(failed_packages, pkg)
    }
  }
  
  if (length(failed_packages) > 0) {
    cat("\n⚠️  The following packages failed to install:\n")
    cat(paste("-", failed_packages, collapse = "\n"), "\n")
    cat("\nThese packages may require system dependencies or may be optional.\n")
  }
  
  return(failed_packages)
}

# Essential packages for package development
essential_packages <- c(
  "devtools",
  "roxygen2", 
  "testthat",
  "pkgbuild",
  "pkgload",
  "usethis"
)

# Core packages from DESCRIPTION Imports
core_packages <- c(
  "cli",
  "dplyr", 
  "ggplot2",
  "rio",
  "rlang",
  "sf",
  "stringr",
  "tibble",
  "tidyr",
  "terra",
  "exactextractr",
  "digest",
  "fs",
  "readr",
  "lubridate"
)

# Optional packages from DESCRIPTION Suggests (may fail on some systems)
optional_packages <- c(
  "arrow",
  "archive",
  "crayon",
  "curl",
  "data.table",
  "ecmwfr",
  "filelock",
  "forcats",
  "foreach", 
  "ggtext",
  "glue",
  "gtranslate",
  "here",
  "httr2",
  "jsonlite",
  "lifecycle",
  "matrixStats",
  "memoise",
  "mockery",
  "ncdf4",
  "nngeo",
  "openxlsx",
  "parzer",
  "progress",
  "purrr",
  "qs2",
  "R.utils",
  "raster",
  "readxl",
  "rnaturalearth",
  "rprojroot",
  "rstudioapi",
  "rvest",
  "scales",
  "shadowtext",
  "slider",
  "stringdist",
  "stringi",
  "vctrs",
  "wesanderson",
  "withr",
  "xml2",
  "zoo"
)

cat("=== Setting up renv for sntutils package ===\n\n")

# Install essential packages first
cat("📦 Installing essential development packages...\n")
failed_essential <- install_packages_safely(essential_packages)

if (length(failed_essential) > 0) {
  stop("❌ Essential packages failed to install. Please fix system dependencies first.")
}

# Install core packages
cat("\n📦 Installing core package dependencies...\n")
failed_core <- install_packages_safely(core_packages)

# Install optional packages (allow failures)
cat("\n📦 Installing optional packages...\n")
failed_optional <- install_packages_safely(optional_packages)

# Update lockfile
cat("\n📸 Taking snapshot of installed packages...\n")
renv::snapshot()

# Summary
cat("\n=== Installation Summary ===\n")
cat("✅ Essential packages:", length(essential_packages) - length(failed_essential), "/", length(essential_packages), "\n")
cat("✅ Core packages:", length(core_packages) - length(failed_core), "/", length(core_packages), "\n") 
cat("✅ Optional packages:", length(optional_packages) - length(failed_optional), "/", length(optional_packages), "\n")

if (length(failed_core) > 0) {
  cat("\n⚠️  Some core packages failed. You may need to install system dependencies:\n")
  cat("  - macOS: brew install cairo freetype fontconfig gdal geos proj\n")
  cat("  - Ubuntu: sudo apt-get install libcairo2-dev libgdal-dev libgeos-dev libproj-dev\n")
}

# Test basic functionality
cat("\n🧪 Testing basic development workflow...\n")
tryCatch({
  devtools::document()
  devtools::load_all()
  cat("✅ Package development workflow is working!\n")
}, error = function(e) {
  cat("❌ Development workflow test failed:", e$message, "\n")
})

cat("\n🎉 renv setup complete! Use 'devtools::load_all()' to start development.\n")