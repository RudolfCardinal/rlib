#!/usr/bin/env Rscript

# =============================================================================
# dplyr (part of tidyverse)
# =============================================================================

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

library(dplyr)
stopifnot(packageVersion("dplyr") >= package_version("1.2.0"))
