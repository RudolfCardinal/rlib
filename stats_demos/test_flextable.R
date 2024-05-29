# test_flextable.R

library(data.table)
library(flextable)
library(ftExtra)  # https://cran.r-project.org/web/packages/ftExtra/vignettes/format_columns.html
library(officer)  # for prop_section, etc.
library(tidyverse)
RLIB_PREFIX <- "/srv/cardinal_rlib/"
source(paste0(RLIB_PREFIX, "miscfile.R"))
source(paste0(RLIB_PREFIX, "miscstat.R"))
source(paste0(RLIB_PREFIX, "miscresults.R"))


# =============================================================================
# Constants
# =============================================================================

SCRIPT_DIR <- miscfile$current_script_directory()
OUTPUT_DOCX <- file.path(SCRIPT_DIR, "test_flextable.docx")


# =============================================================================
# Made-up data and formatting demostrations
# =============================================================================

set.seed(1)

total_placebo <- 520
n_explosions_placebo <- 500
n_implosions_placebo <- 250
weight_placebo <- rnorm(n = total_placebo, mean = 40.25, sd = 2)
height_placebo <- rnorm(n = total_placebo, mean = 1.4, sd = 0.1)
n_dullness_measured_placebo <- 5
dullness_placebo <- rnorm(n = n_dullness_measured_placebo, mean = 7.7, sd = 0.1)

total_drug <- 525
n_explosions_drug <- 300
n_implosions_drug <- 251
weight_drug <- rnorm(n = total_drug, mean = 65.78, sd = 2)
height_drug <- rnorm(n = total_drug, mean = -1.8, sd = 0.1)
n_dullness_measured_drug <- 5
dullness_drug <- rnorm(n = n_dullness_measured_drug, mean = 7.7, sd = 0.1)

fakedata <- data.table(
    groupname = c("placebo", "drug", "comparison"),
    n = c(total_placebo, total_drug, "–"),
    explosions = c(
        mk_n_percent(n_explosions_placebo, total_placebo),
        mk_n_percent(n_explosions_drug, total_drug),
        mk_chisq_contingency(
            c(total_placebo, n_explosions_placebo),
            c(total_drug, n_explosions_drug)
        )
    ),
    implosions = c(
        mk_n_percent(n_implosions_placebo, total_placebo),
        mk_n_percent(n_implosions_drug, total_drug),
        mk_chisq_contingency(
            c(total_placebo, n_implosions_placebo),
            c(total_drug, n_implosions_drug)
        )
    ),
    weight = c(
        mk_mean_sd(weight_placebo),
        mk_mean_sd(weight_drug),
        mk_t_test(weight_placebo, weight_drug)
    ),
    height = c(
        mk_mean_ci(height_placebo),
        mk_mean_ci(height_drug),
        mk_t_test(height_placebo, height_drug)
    ),
    n_dullness_measured = c(
        n_dullness_measured_placebo,
        n_dullness_measured_drug,
         "–"
     ),
    dullness = c(
        mk_mean_ci(dullness_placebo),
        mk_mean_ci(dullness_drug),
        mk_t_test(dullness_placebo, dullness_drug)
    )
)
# print(fakedata)
td <- data.table::transpose(
    fakedata, make.names = "groupname", keep.names = "variable"
)
# print(td)


# =============================================================================
# Formatting demostrations
# =============================================================================

ft <- (
    flextable(td)
    %>% ftExtra::colformat_md()  # apply markdown
    %>% autofit()  # size columns
    %>% set_caption("My first flextable")
)
print(ft)
flextable::save_as_docx(
    `Table 1` = ft,
    `Table 1 again` = ft,
    path = OUTPUT_DOCX,
    align = "left",
    pr_section = prop_section(  # from "officer" package
        # These do not use all the width appropriately, despite correct margin
        # settings (with flextable 0.9.4 and officer 0.6.3):
        page_size = page_size(orient = "landscape")  # default is A4 portrait
        # page_size = page_size(width = 29.7 / 2.54, height = 21 / 2.54, orient = "portrait")  # doesn't work properly
        # page_size = page_size(width = 29.7 / 2.54, height = 21 / 2.54, orient = "landscape")  # doesn't work properly
        # page_margins = page_mar(...),  # default is 1" margins
    )
)
