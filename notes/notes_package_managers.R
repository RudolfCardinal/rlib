#!/usr/bin/env Rscript

# =============================================================================
# Quick summary
# =============================================================================
#
# -----------------------------------------------------------------------------
# INSTALLING PACKAGES
# -----------------------------------------------------------------------------
#
#   utils::install.packages(c("PACKAGE1", "PACKAGE2"), ...)
#       Core R.
#   pak::pak(c("PACKAGE1", "PACKAGE2"), ...)
#       One of the fastest and cleanest.
#
# -----------------------------------------------------------------------------
# CHECKING AND LOADING INSTALLED PACKAGES
# -----------------------------------------------------------------------------
#
#   base::loadNamespace("PACKAGE")
#       Checks that a package is installed. Raises an error if it fails.
#       If it succeeds, you can call PACKAGE::fn() with the prefix.
#   base::requireNamespace("PACKAGE")
#       Checks that a package is installed. Raises an error if it fails.
#       If it succeeds, you can call PACKAGE::fn() with the prefix.
#   base::library(PACKAGE)
#       Attaches package to search path. Raises an error if it fails.
#       If it succeeds, you can now use PACKAGE::fn() as fn().
#   base::require(PACKAGE)
#       Attaches package to search path. Returns FALSE if it fails.
#       If it succeeds, you can now use PACKAGE::fn() as fn().
#
#   utils::sessionInfo()
#       Show currently attached package versions.
#   utils::packageVersion("PACKAGE")
#       Gets version of an installed package, as a package_version() object.
#   base::package_version("1.16.0")
#       Creates a package_version() object from a string, e.g. for comparison
#       to the result of utils::packageVersion().
#
# -----------------------------------------------------------------------------
# COMPOSITE ACTIONS
# -----------------------------------------------------------------------------
#
#   pacman::p_load(PACKAGE1, PACKAGE2, ...)
#       Loads the packages, like library(), but attempts to install them first
#       if not already present.


# =============================================================================
# Package management systems
# =============================================================================
#
# -----------------------------------------------------------------------------
# base
# -----------------------------------------------------------------------------
#
# Useful for checking in user code, e.g.
#
#   stopifnot(packageVersion("data.table") >= package_version("1.16.0"))
#
# -----------------------------------------------------------------------------
# pacman
# -----------------------------------------------------------------------------
# - https://trinker.github.io/pacman_dev/
#
# For version control -- pacman 0.5.1 is not reliable.
#
#   pacman::p_install_version('<package>', version = '<version>')
#
# ... sometimes just flat-out installs the wrong version. Avoid?
#
# -----------------------------------------------------------------------------
# devtools
# -----------------------------------------------------------------------------
# - https://devtools.r-lib.org/
#
#   devtools::install_version(
#       'PACKAGE',
#       version = 'VERSION',
#       repos = c('SPECIFIC_URL_IF_REQUIRED', getOption('repos')),
#       dependencies = TRUE
#   )"
#
# But they have deprecated (some of) themselves in favour of pak:
#
#   https://devtools.r-lib.org/reference/install-deprecated.html
#
# -----------------------------------------------------------------------------
# packrat
# -----------------------------------------------------------------------------
# - https://rstudio.github.io/packrat/
#
# -----------------------------------------------------------------------------
# pak
# -----------------------------------------------------------------------------
# - https://pak.r-lib.org/
#
# Defaults towards the latest versions. But you can be specific:
#
#   pak::pak("PACKAGE")
#   pak::pak("PACKAGE@VERSION")
#   pak::pak("PACKAGE@>=VERSION")
#       # though not really: "! Version ranges are not implemented yet."
#
# GitHub installation: e.g.
#
#   pak::pak("tidyverse/tibble")
#
# See:
#   ?"Package sources"
