# test_flextable.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    data.table,
    flextable,  # for creating tabular publication output
    ftExtra,  # for markdown in flextable tables
    officer,  # for embedding flextable tables in Word documents
    tidyverse
)

# RLIB_PREFIX <- "/srv/cardinal_rlib/"
RLIB_PREFIX <- ""
source(paste0(RLIB_PREFIX, "miscfile.R"))
source(paste0(RLIB_PREFIX, "miscstat.R"))
source(paste0(RLIB_PREFIX, "miscresults.R"))

# Re mutating rows:
# - https://github.com/tidyverse/dplyr/issues/4050

# =============================================================================
# Constants
# =============================================================================

SCRIPT_DIR <- miscfile$current_script_directory()
OUTPUT_DOCX <- file.path(SCRIPT_DIR, "test_flextable.docx")


# =============================================================================
# Made-up data and formatting demonstrations
# =============================================================================

set.seed(1)

# -----------------------------------------------------------------------------
# Individual-level data
# -----------------------------------------------------------------------------
n_placebo <- 2201
n_drug <- 1258
fakedata <- data.table(
    group = c(
        rep("placebo", n_placebo),
        rep("drug", n_drug)
    ),
    exploded = as.integer(c(
        miscstat$coin(rep(0.2, n_placebo)),
        miscstat$coin(rep(0.1, n_drug))
    )),
    imploded = as.integer(c(
        miscstat$coin(rep(0.2,  n_placebo)),
        miscstat$coin(rep(0.25, n_drug))
    )),
    weight = c(
        rnorm(n = n_placebo, mean = 60.25, sd = 2),
        rnorm(n = n_drug,    mean = 65.78, sd = 2)
    ),
    height = c(
        rnorm(n = n_placebo, mean = 1.7, sd = 0.1),
        rnorm(n = n_drug,    mean = 1.7, sd = 0.1)
    ),
    response = c(
        rnorm(n = n_placebo, mean =  0.2, sd = 3),
        rnorm(n = n_drug,    mean = -0.2, sd = 3)
    ),
    dullness = c(
        floor(runif(n = n_placebo, min = 10, max = 40)),
        floor(runif(n = n_drug,    min = 11, max = 40))
    )
)

# -----------------------------------------------------------------------------
# Group-level numerical summaries
# -----------------------------------------------------------------------------

summary <- (
    fakedata
    %>% group_by(group)
    %>% summarize(
        # Numerical
        n = n(),
        n_explosions = sum(exploded),
        n_not_explosions = n - n_explosions,
        n_implosions = sum(imploded),
        n_not_implosions = n - n_implosions,
        # Textual
        N = fmt_int(n),
        explosions = mk_n_percent(n_explosions, n),
        implosions = mk_n_percent(n_implosions, n),
        weight = mk_mean_sd(weight),
        height = mk_mean_sd(height),
        response = mk_mean_ci(response),
        dullness = mk_median_range(dullness)
    )
    %>% as.data.table()
)
# Keep in this "variables as columns" format for calculation. Only transpose
# once we don't care about a mix of text and numbers.

# -----------------------------------------------------------------------------
# Transposed, textual summaries
# -----------------------------------------------------------------------------

tsumm <- (
    summary
    %>% select(-c(
        # Columns to remove
        "n",
        "n_explosions", "n_not_explosions",
        "n_implosions", "n_not_implosions"
    ))
    %>% data.table::transpose(make.names = "group", keep.names = "variable")
    %>% mutate(
        comparison = case_when(
            # Add comparisons.
            # Use x = drug, y = placebo, giving "drug - placebo" for the tests.
            variable == "explosions" ~ mk_chisq_contingency(
                c(summary[group == "drug"]$n_explosions,
                  summary[group == "drug"]$n_not_explosions),
                c(summary[group == "placebo"]$n_explosions,
                  summary[group == "placebo"]$n_not_explosions)
            ),
            variable == "implosions" ~ mk_chisq_contingency(
                c(summary[group == "drug"]$n_implosions,
                  summary[group == "drug"]$n_not_implosions),
                c(summary[group == "placebo"]$n_implosions,
                  summary[group == "placebo"]$n_not_implosions)
            ),
            variable == "weight" ~ mk_t_test(
                fakedata[group == "drug"   ]$weight,
                fakedata[group == "placebo"]$weight
            ),
            variable == "height" ~ mk_t_test(
                fakedata[group == "drug"   ]$height,
                fakedata[group == "placebo"]$height
            ),
            variable == "response" ~ mk_t_test(
                fakedata[group == "drug"   ]$response,
                fakedata[group == "placebo"]$response
            ),
            variable == "dullness" ~ mk_wilcoxon_test(
                fakedata[group == "drug"   ]$dullness,
                fakedata[group == "placebo"]$dullness
            ),
            TRUE ~ miscresults$EN_DASH
        ),
        # Make variable names prettier
        variable = case_when(
            variable == "explosions" ~ "Explosions",
            variable == "implosions" ~ "Implosions",
            variable == "weight" ~ "Weight (kg)",
            variable == "height" ~ "Height (m)",
            variable == "response" ~ "Response (response units)",
            variable == "dullness" ~ "Dullness (bishops)",
            TRUE ~ variable
        )
    )
    %>% add_row(
        variable = "Primary outcomes",
        .before = 2
    )
    %>% add_row(
        variable = "Secondary outcomes",
        .before = 5
    )
)
# Get the groups in the right order:
setcolorder(tsumm, c("variable", "placebo", "drug", "comparison"))
# Make it prettier:
colnames(tsumm) <- c("Variable", "Placebo", "Drug", "Comparison")


# =============================================================================
# Formatting demostrations
# =============================================================================

flextable::set_flextable_defaults(
    font.family = "Arial",
    font.size = 12,
    border.color = "gray",
    digits = 3,  # usually significant figures
    big.mark = ","  # thousands separator
)
ft <- (
    tsumm
    %>% flextable()
    %>% ftExtra::colformat_md()  # apply markdown
    %>% autofit()  # size columns
    %>% set_caption("My first flextable")
    # Mark significant differences in bold. To make this harder, note that
    # asterisks are present in the markdown text! So we're looking for
    # whitespace (\s), one or more asterisks (\*+), end of string ($). Then
    # additional backslashes for R.
    %>% bold(
        i = ~ miscresults$detect_significant_in_result_str(Comparison),
        j = c("Drug")
    )
    # Make some subheadings bold, in a primitive way.
    %>% bold(i = ~ str_detect(Variable, "outcomes"))
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
