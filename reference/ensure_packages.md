# Ensure Required Packages are Installed and Loaded

This function checks if the specified packages are installed, prompts
the user to install missing packages, and loads all packages into the
current session. In non-interactive sessions, installation is skipped
with a warning.

## Usage

``` r
ensure_packages(pkgs)
```

## Arguments

- pkgs:

  A character vector of package names to check and potentially install.

## Value

Invisibly returns NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
ensure_packages(c("dplyr", "ggplot2"))
} # }
```
