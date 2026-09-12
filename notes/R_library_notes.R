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


# =============================================================================
# dplyr (part of tidyverse)
# =============================================================================
#
# -----------------------------------------------------------------------------
# dplyr::replace_values()
# dplyr::recode_values()
# -----------------------------------------------------------------------------
#
#   Both require dplyr 1.2.0.
#
# -----------------------------------------------------------------------------
# dplyr::case_when()
# -----------------------------------------------------------------------------
#
#   The ".default" option is available, and preferred, from dplyr 1.1.0; see
#   https://stackoverflow.com/questions/78973291/.
#   Older versions: TRUE ~ ...
#   Newer versions: .default = ...
#       ... "TRUE ~" still works, but is deprecated.

library(dplyr)
stopifnot(packageVersion("dplyr") >= package_version("1.2.0"))


# =============================================================================
# data.table
# =============================================================================
#
# -----------------------------------------------------------------------------
# Joins in data.table (RIGHT OUTER JOIN, INNER JOIN, etc.)
# -----------------------------------------------------------------------------
#
# See data_table_joins.R.
#
# -----------------------------------------------------------------------------
# Progress bars
# -----------------------------------------------------------------------------
#
# - Manual methods: https://github.com/Rdatatable/data.table/issues/1409
#
# - Automatic method: available from version 1.16.0 (25 Aug 2024).
#   See https://cran.r-project.org/web/packages/data.table/news/news.html.
#   Availability is indicated by the "showProgress" option in ?data.table,
#   which defaults to:
#       showProgress = getOption("datatable.showProgress", interactive())]

library(data.table)
stopifnot(packageVersion("data.table") >= package_version("1.16.0"))

test_data_table_progress_bar <- function(simple_by = TRUE, with_assign = TRUE)
{
    # Tested with v1.18.6.1, 12 Sep 2006.

    n <- 10
    d <- data.table(grouper = LETTERS[1:n], value = 1:n)

    groupfn1 <- function(BY, SD, sleep = 1) {
        # cat("- Sleeping for", sleep, "\n")
        Sys.sleep(sleep)
        return(list(p = 1, q = 1))
    }

    if (simple_by) {
        cat("- Testing data.table progress bar: (1) Standard 'by'\n")
        g = d[, groupfn1(.BY, .SD), by = grouper]
        print(g)
        cat("  ... outcome: DISPLAYS progress bar.\n")
    }

    if (with_assign) {
        cat("- Testing data.table progress bar: (1) Direct ':='\n")
        dc <- copy(d)
        dc[, c("p", "q") := groupfn1(.BY, .SD), by = grouper]
        print(dc)
        cat("  ... outcome: DOES NOT DISPLAY progress bar.\n")
    }
}
