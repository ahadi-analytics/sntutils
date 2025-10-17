# renv Setup Guide for sntutils

## Overview

This project uses `renv` to manage R package dependencies and create reproducible environments. This ensures all team members work with the same package versions and reduces "it works on my machine" issues.

## Quick Start

### First Time Setup

1. **Clone the repository**:
   ```bash
   git clone https://github.com/ahadi-analytics/sntutils.git
   cd sntutils
   ```

2. **Open R in the project directory**:
   ```r
   # renv will activate automatically due to .Rprofile
   # If packages are missing, restore them:
   renv::restore()
   ```

3. **Install the package in development mode**:
   ```r
   devtools::load_all()
   ```

## renv Profiles

We have configured different profiles for different use cases:

### Default Profile (Current Environment)
- **Use case**: General development and testing
- **Contains**: All packages currently in the lockfile
- **Activate**: Automatic when opening R in project

### Development Profile (`dev`)
- **Use case**: Package development, testing, documentation
- **Contains**: Core packages + development tools (devtools, testthat, roxygen2, pkgdown) + optional aesthetic packages
- **Activate**: 
  ```r
  renv::activate(profile = "dev")
  renv::restore()
  ```

### Production Profile (`prod`)
- **Use case**: Minimal environment for using sntutils in production
- **Contains**: Only essential packages from Imports in DESCRIPTION
- **Activate**:
  ```r
  renv::activate(profile = "prod")
  renv::restore()
  ```

## Common Commands

### Daily Development
```r
# Check package status
renv::status()

# Load all package functions for testing
devtools::load_all()

# Run tests
devtools::test()

# Update documentation
devtools::document()
```

### Managing Dependencies

```r
# Install a new package
install.packages("newpackage")

# Update lockfile after adding dependencies
renv::snapshot()

# Remove unused packages
renv::clean()

# Check for package updates
renv::update()
```

### Working with Profiles

```r
# Switch to development profile
renv::activate(profile = "dev")
renv::restore()

# Switch back to default
renv::activate(profile = NULL)
renv::restore()

# See current profile
renv::project()
```

### Troubleshooting

```r
# Reset to lockfile state
renv::restore()

# Rebuild package cache
renv::rebuild()

# Check renv status
renv::diagnostics()
```

## Team Collaboration

### For New Team Members

1. Clone the repository
2. Open R in the project directory
3. Run `renv::restore()` to install all required packages
4. Use `devtools::load_all()` to start working

### For Package Maintainers

1. **Adding dependencies**: 
   - Add to DESCRIPTION file
   - Run `renv::snapshot()` to update lockfile
   - Commit both DESCRIPTION and renv.lock

2. **Updating dependencies**:
   - Run `renv::update()` 
   - Test thoroughly
   - Run `renv::snapshot()` to save changes
   - Commit updated renv.lock

3. **Managing profiles**:
   - Update profile lockfiles when adding development tools
   - Keep production profile minimal

## Configuration

### Settings (renv/settings.json)
- `auto.snapshot: false` - Manual control over snapshots
- `snapshot.type: "explicit"` - Only include explicitly used packages
- `use.cache: true` - Share packages across projects to save space

### Files to Commit
- ✅ `renv.lock` - Main lockfile
- ✅ `renv/settings.json` - Project settings  
- ✅ `renv/profiles/*/renv.lock` - Profile lockfiles
- ✅ `.Rprofile` - renv activation
- ❌ `renv/library/` - Local package installation (ignored)
- ❌ `renv/staging/` - Temporary files (ignored)

## Benefits

1. **Reproducibility**: Everyone uses the same package versions
2. **Isolation**: Project dependencies don't conflict with other projects
3. **Efficiency**: Shared package cache saves disk space
4. **Flexibility**: Different profiles for different use cases
5. **Collaboration**: Easy onboarding for new team members

## Integration with Package Development

### Testing Pipeline
```r
# Full development workflow
renv::activate(profile = "dev")
devtools::load_all()
devtools::test()
devtools::check()
devtools::document()
```

### Building Package
```r
# Switch to production environment for clean build
renv::activate(profile = "prod")
devtools::build()
devtools::install()
```

## FAQ

**Q: Can I use my global R packages?**
A: renv isolates the project, but you can access global packages by deactivating renv temporarily with `renv::deactivate()`.

**Q: What if a package isn't available on CRAN?**
A: renv supports GitHub, Bioconductor, and other sources. Install with `remotes::install_github()` then snapshot.

**Q: How do I update just one package?**
A: `renv::install("packagename")` then `renv::snapshot()`.

**Q: Can I ignore certain packages?**
A: Add them to `ignored.packages` in renv/settings.json.

## Support

For renv-specific issues:
- [renv documentation](https://rstudio.github.io/renv/)
- [renv GitHub issues](https://github.com/rstudio/renv/issues)

For sntutils-specific issues:
- Create an issue in the sntutils repository
- Contact the package maintainer