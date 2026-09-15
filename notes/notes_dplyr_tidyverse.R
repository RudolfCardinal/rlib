#!/usr/bin/env Rscript

# =============================================================================
# dplyr (part of tidyverse)
# =============================================================================

library(dplyr)

# -----------------------------------------------------------------------------
# dplyr::first()
# dplyr::last()
# -----------------------------------------------------------------------------
#
# Both return NA if there is no appropriate value.
# For example:
#       x <- 1:5
#       first(which(x == 3))  # 3
#       first(which(x == 7))  # NA


# -----------------------------------------------------------------------------
# dplyr::replace_values()
# dplyr::recode_values()
# -----------------------------------------------------------------------------
#
#   Both require dplyr 1.2.0.


# -----------------------------------------------------------------------------
# dplyr::case_when()
# -----------------------------------------------------------------------------
#
#   The ".default" option is available, and preferred, from dplyr 1.1.0; see
#   https://stackoverflow.com/questions/78973291/.
#   Older versions: TRUE ~ ...
#   Newer versions: .default = ...
#       ... "TRUE ~" still works, but is deprecated.

stopifnot(packageVersion("dplyr") >= package_version("1.2.0"))
