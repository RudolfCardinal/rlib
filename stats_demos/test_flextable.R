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
RLIB_PREFIX <- ""  # use when editing miscresults.R
source(paste0(RLIB_PREFIX, "miscfile.R"))
source(paste0(RLIB_PREFIX, "miscstat.R"))
source(paste0(RLIB_PREFIX, "miscresults.R"))

# Set up flextable defaults before ANY functions using flextable formatting.

flextable::set_flextable_defaults(
    font.family = "Arial",
    font.size = 10,
    border.color = "gray",
    digits = 3,  # usually significant figures
    big.mark = ",",  # thousands separator
    na_str = "NA",
    nan_str = "NaN"
)


# =============================================================================
# Constants
# =============================================================================

SCRIPT_DIR <- miscfile$current_script_directory()
OUTPUT_DOCX <- file.path(SCRIPT_DIR, "test_flextable.docx")

FOOTNOTE_SEP <- " "
FOOTNOTE_OPTIONS <- ftExtra::footnote_options(
    ref = "a",  # E.g. "a" (letters), "i" (Roman), 1 (Arabic), "*", etc.
    prefix = "",  # Before the numbered/lettered part.
    suffix = "",  # After the numbered/lettered part.
    start = 1,  # e.g. 2 = start at "b", "ii", "2", etc.
    inline = TRUE,  # Footnotes in same line within table footer?
    sep = FOOTNOTE_SEP  # Between footnotes. Only applicable if inline == TRUE.
)


# =============================================================================
# Made-up data and formatting demonstrations
# =============================================================================

set.seed(1)

# -----------------------------------------------------------------------------
# Individual-level data
# -----------------------------------------------------------------------------
n_placebo <- 2201
n_drug <- 1258
fd1 <- data.table(  # fd, fake data
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
        floor(runif(n = n_placebo, min = -10, max = 40)),
        floor(runif(n = n_drug,    min =  -9, max = 40))
    )
)

# -----------------------------------------------------------------------------
# Group-level numerical summaries
# -----------------------------------------------------------------------------

suppression_threshold <- 300  # silly; would normally be e.g. 10
summary1 <- (
    fd1
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
        implosions_suppressed = mk_n_percent(
            n_implosions,
            n,
            min_threshold = suppression_threshold
        ),
        implosions_vague = fmt_n_percent_low_high(
            n_implosions - 5,
            n_implosions + 5,
            n
        ),
        weight = mk_mean_sd(weight),
        height = mk_mean_sd(height),
        response_mean = mk_mean_ci(response),
        response_median = mk_median_range(response),
        dullness = mk_median_range(dullness)
    )
    %>% as.data.table()
)
# Keep in this "variables as columns" format for calculation. Only transpose
# once we don't care about a mix of text and numbers.

# -----------------------------------------------------------------------------
# Transposed, textual summaries
# -----------------------------------------------------------------------------

# Re mutating rows:
# - https://github.com/tidyverse/dplyr/issues/4050

tsumm1 <- (
    summary1
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
                c(summary1[group == "drug"]$n_explosions,
                  summary1[group == "drug"]$n_not_explosions),
                c(summary1[group == "placebo"]$n_explosions,
                  summary1[group == "placebo"]$n_not_explosions)
            ),
            variable == "implosions" ~ mk_chisq_contingency(
                c(summary1[group == "drug"]$n_implosions,
                  summary1[group == "drug"]$n_not_implosions),
                c(summary1[group == "placebo"]$n_implosions,
                  summary1[group == "placebo"]$n_not_implosions)
            ),
            variable == "weight" ~ mk_t_test(
                fd1[group == "drug"   ]$weight,
                fd1[group == "placebo"]$weight
            ),
            variable == "height" ~ mk_t_test(
                fd1[group == "drug"   ]$height,
                fd1[group == "placebo"]$height
            ),
            variable == "response_mean" ~ mk_t_test(
                fd1[group == "drug"   ]$response,
                fd1[group == "placebo"]$response
            ),
            variable == "dullness" ~ mk_wilcoxon_test(
                fd1[group == "drug"   ]$dullness,
                fd1[group == "placebo"]$dullness
            ),
            TRUE ~ miscresults$EN_DASH
        ),
        # Make variable names prettier
        variable = case_when(
            variable == "explosions" ~ "Explosions",
            variable == "implosions" ~ "Implosions",
            variable == "implosions_suppressed" ~ "Implosions (suppressed)",
            variable == "implosions_vague" ~ "Implosions (vague)",
            variable == "weight" ~ "Weight (kg)",
            variable == "height" ~ "Height (m)",
            variable == "response_mean" ~ "Response (response units) (mean)",
            variable == "response_median" ~ "Response (response units) (median)",
            variable == "dullness" ~ "Dullness (bishops)",
            TRUE ~ variable
        )
    )
    %>% add_row(
        variable = "Primary outcomes ^[First footnote.]",
        .before = 2
    )
    %>% add_row(
        variable = "Secondary outcomes ^[Second footnote.]",
        .before = 6
    )
)
# Get the groups in the right order:
setcolorder(tsumm1, c("variable", "placebo", "drug", "comparison"))
# Make it prettier:
colnames(tsumm1) <- c("Variable", "Placebo", "Drug", "Comparison")


# =============================================================================
# A three-group example
# =============================================================================

n_lowdose <- 1010
n_highdose <- 1258
fd2 <- data.table(
    group = c(
        rep("placebo", n_placebo),
        rep("lowdose", n_lowdose),
        rep("highdose", n_highdose)
    ),
    sbp = c(
        rnorm(n = n_placebo,  mean = 138, sd = 2),
        rnorm(n = n_lowdose,  mean = 130, sd = 2),
        rnorm(n = n_highdose, mean = 87, sd = 2)  # unhappy occurrence
    ),
    collapsed = as.integer(c(
        miscstat$coin(rep(0.05, n_placebo)),
        miscstat$coin(rep(0.06, n_lowdose)),
        miscstat$coin(rep(0.15, n_highdose))
    ))
)
s2 <- (
    fd2
    %>% group_by(group)
    %>% summarize(
        # Numerical
        n = n(),
        n_collapsed = sum(collapsed),
        n_not_collapsed = n - n_collapsed,
        # Textual
        N = fmt_int(n),
        collapsed = mk_n_percent(n_collapsed, n),
        sbp = mk_mean_sd(sbp)
    )
    %>% as.data.table()
)
t2 <- (
    s2
    %>% select(-c(
        # Columns to remove
        "n",
        "n_collapsed",
        "n_not_collapsed",
    ))
    %>% data.table::transpose(make.names = "group", keep.names = "variable")
    %>% mutate(
        comparison = case_when(
            # Add comparisons.
            # Use x = drug, y = placebo, giving "drug - placebo" for the tests.
            variable == "collapsed" ~ mk_chisq_contingency(
                c(s2[group == "placebo" ]$n_collapsed,
                  s2[group == "lowdose" ]$n_collapsed,
                  s2[group == "highdose"]$n_collapsed),
                c(s2[group == "placebo" ]$n_not_collapsed,
                  s2[group == "lowdose" ]$n_not_collapsed,
                  s2[group == "highdose"]$n_not_collapsed)
            ),
            variable == "sbp" ~ mk_oneway_anova(fd2$sbp, fd2$group),
            TRUE ~ miscresults$EN_DASH
        ),
        # Make variable names prettier
        variable = case_when(
            variable == "collapsed" ~ paste(
                "Collapsed",
                "line_2",
                "line_3",
                sep = miscresults$NEWLINE
            ),
            variable == "sbp" ~ "Systolic BP (mmHg)",
            TRUE ~ variable
        )
    )
)
setcolorder(t2, c(
    "variable", "placebo", "lowdose", "highdose", "comparison"
))
colnames(t2) <- c(
    "Variable", "Placebo", "Low dose", "High dose", "Comparison"
)


# =============================================================================
# A linear model
# =============================================================================

n_per_group <- 100
SEX_FEMALE <- "Female"
SEX_MALE <- "Male"
DRUG_PLACEBO <- "Placebo"
DRUG_LOW <- "LowDose"
DRUG_HIGH <- "HighDose"
fd3 <- data.table(do.call(
    "rbind",
    replicate(
        n_per_group,
        expand.grid(
            sex = c(SEX_FEMALE, SEX_MALE),
            drug = c(DRUG_PLACEBO, DRUG_LOW, DRUG_HIGH)
        ),
    simplify = FALSE)
))
fd3[, age := rnorm(n = nrow(fd3), mean = 40, sd = 10)]
# These should be recovered in the table, ft3a:
COEFF_INTERCEPT <- 100.0
COEFF_AGE <- 0.2  # per year
COEFF_MALE <- -3  # versus female (reference)
COEFF_DRUG_LOW <- 5  # versus placebo (reference)
COEFF_DRUG_HIGH <- -5  # versus placebo (reference)
fd3[, y_start := COEFF_INTERCEPT]
fd3[, y_age := COEFF_AGE * age]
fd3[, y_sex := case_when(
    sex == SEX_FEMALE ~ 0,  # must be zero for recovered intercept to be right
    sex == SEX_MALE ~ COEFF_MALE,
    TRUE ~ NA_real_
)]
# ... DO NOT USE:
# as.numeric(plyr::mapvalues(
#   sex, from = c(SEX_FEMALE, SEX_MALE), to = c(0, COEFF_MALE)
# ))
# ... this uses a factor internally and yields 1, 2 etc.
fd3[, y_drug := case_when(
    drug == DRUG_PLACEBO ~ 0,  # must be zero as above
    drug == DRUG_LOW ~ COEFF_DRUG_LOW,
    drug == DRUG_HIGH ~ COEFF_DRUG_HIGH,
    TRUE ~ NA_real_
)]
fd3[, err := rnorm(n = nrow(fd3), mean = 0, sd = 2.0)]
fd3[, performance := y_start + y_age + y_sex + y_drug + err]
fd3[,
    succeeded := as.integer(performance > mean(performance))
]
M3_PREDICTOR_REPLACEMENTS <- c(
    # Factors
    "age" = "Age",
    "drug" = "Drug",
    "sex" = "Sex",
    # Levels
    "LowDose" = "Low dose",
    "HighDose" = "High dose"
)


# =============================================================================
# Formatting demostrations
# =============================================================================

ft1 <- (
    tsumm1
    %>% flextable()
    %>% ftExtra::colformat_md(
        .footnote_options = FOOTNOTE_OPTIONS
    )  # apply markdown
    %>% flextable::valign(valign = "top")  # align all cells top
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
    # For a footnote with multiple "citations":
    %>% footnote(
        # i and j represent row, column pairs
        i = c(3, 3),  # rows
        j = c(2, 3),  # columns
        value = as_paragraph("Hello, world!"),
        ref_symbols = c(" †"),
        part = "body",
        inline = TRUE,
        sep = FOOTNOTE_SEP
    )
    %>% footnote(
        # i and j represent row, column pairs
        i = c(4, 4),  # rows
        j = c(2, 3),  # columns
        value = as_paragraph("May be volatile."),
        ref_symbols = c(" ‡"),
        part = "body",
        inline = TRUE,
        sep = FOOTNOTE_SEP
    )
)
ft2 <- (
    t2
    %>% flextable()
    %>% ftExtra::colformat_md(
        .footnote_options = FOOTNOTE_OPTIONS
    )  # apply markdown
    %>% flextable::valign(valign = "top")  # align all cells top
    %>% autofit()  # size columns
    %>% set_caption("Three-group table")
    %>% bold(
        i = ~ miscresults$detect_significant_in_result_str(Comparison),
        j = c("Placebo", "Low dose", "High dose")
    )
)
m3a <- mk_model_anova_coeffs(
    model_fn = lm,
    formula = performance ~ age + drug * sex,
    data = fd3,
    predictor_replacements = M3_PREDICTOR_REPLACEMENTS
)
ft3a <- m3a$table_flex
m3b <- mk_model_anova_coeffs(
    model_fn = glm,
    formula = succeeded ~ age + drug * sex,
    family = binomial(link = "logit"),
    data = fd3,
    predictor_replacements = M3_PREDICTOR_REPLACEMENTS
)
ft3b <- m3b$table_flex
m3c <- mk_model_anova_coeffs(
    model_fn = glm,
    formula = succeeded ~ age + drug * sex,
    family = binomial(link = "logit"),
    data = fd3,
    predictor_replacements = M3_PREDICTOR_REPLACEMENTS,
    squish_up_level_rows = TRUE  # new here
)
ft3c <- m3c$table_flex
m3d <- mk_model_anova_coeffs(
    model_fn = glm,
    formula = succeeded ~ age + drug * sex,
    family = binomial(link = "logit"),
    data = fd3,
    predictor_replacements = M3_PREDICTOR_REPLACEMENTS,
    squish_up_level_rows = TRUE,
    include_reference_levels = FALSE  # new here
)
ft3d <- m3d$table_flex


# =============================================================================
# Saving and printing
# =============================================================================

cat(paste0("Saving to ", OUTPUT_DOCX, "...\n"))
flextable::save_as_docx(
    `Table 1` = ft1,
    `Table 1 again` = ft1,  # a second copy
    `Table 2` = ft2,
    `Table 3a (linear)` = ft3a,
    `Table 3b (logistic)` = ft3b,
    `Table 3c (as 3b but squished up)` = ft3c,
    `Table 3c (as 3c but no reference levels)` = ft3d,
    path = OUTPUT_DOCX,  # file will be created or overwritten
    align = "left",  # table (and caption) within page (not text within table)
    pr_section = prop_section(  # from "officer" package
        # These do not use all the width appropriately, despite correct margin
        # settings (with flextable 0.9.4 and officer 0.6.3):
        page_size = page_size(orient = "landscape")  # default is A4 portrait
        # page_size = page_size(orient = "portrait")  # default is A4 portrait
        # page_size = page_size(width = 29.7 / 2.54, height = 21 / 2.54, orient = "portrait")  # doesn't work properly
        # page_size = page_size(width = 29.7 / 2.54, height = 21 / 2.54, orient = "landscape")  # doesn't work properly
        # page_margins = page_mar(...),  # default is 1" margins
    )
)

# PROMPT <- "Press [Enter] to see next table..."
# print(ft1); readline(PROMPT)
# print(ft2); readline(PROMPT)
# print(ft3a); readline(PROMPT)
# print(ft3b); readline(PROMPT)
# print(ft3c); readline(PROMPT)
# print(ft3d)
