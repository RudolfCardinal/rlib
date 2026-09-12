#!/usr/bin/env Rscript

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
# ... sometimes just flat-out installs the wrong version. Avoid.
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
# But they have deprecated themselves in favour of pak:
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
