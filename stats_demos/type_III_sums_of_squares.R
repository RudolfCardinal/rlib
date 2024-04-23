#!/usr/bin/Rscript
#
# type_III_sums_of_squares.R
#
# A reminder, 17 Apr 2024.
#
# - For within-subjects/mixed designs, using lmerTest::lmer() as a wrapper for
#   lme4::lmer() does the job automatically, and provides type III SS helpfully.
#   (For example, see nhs_data_survey/output/output.txt.) This does not work
#   for
#
# - For between-subjects designs, there's the usual choice, per
#   https://rcardinal.ddns.net/statistics/R/anova.html:
#
#       - lm() but with special contrasts set and the drop1() command;
#       - car::Anova() but with special contrasts
#
# - Shown here:
#
#   - define contr.sum.keepnames()
#   - use it, via set_type3_contrasts_b()
#   - then car::Anova(lm(...), type = "III")
#
# - See also
#   - http://myowelt.blogspot.com/2008/05/obtaining-same-anova-results-in-r-as-in.html
#   - https://stackoverflow.com/questions/10808853/why-does-changing-contrast-type-change-row-labels-in-r-lm-summary

# =============================================================================
# Libraries
# =============================================================================

library(data.table)
library(tidyverse)

library(car)
library(ez)
library(lme4)
library(lmerTest)

# =============================================================================
# Helper functions for options
# =============================================================================
# See ?contrast

saved_options_contrasts <- getOption("contrasts")
# ... c(unordered = "contr.treatment", ordered = "contr.poly")
revert_contrasts <- function() {
    options(contrasts = saved_options_contrasts)
}
set_r_default_contrasts <- function() {
    options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
}

set_type3_contrasts <- function() {
    options(contrasts = c(unordered = "contr.sum", ordered = "contr.poly"))
}
set_type3_contrasts_keepnames <- function() {
    options(contrasts = c(unordered = "contr.sum.keepnames", ordered = "contr.poly"))
}

# =============================================================================
# Useful functions!
# =============================================================================

contr.sum.keepnames <- function(...) {
    # https://stackoverflow.com/questions/10808853/why-does-changing-contrast-type-change-row-labels-in-r-lm-summary
    conS <- contr.sum(...)
    colnames(conS) <- rownames(conS)[-length(rownames(conS))]
    conS
}

# =============================================================================
# Generate specimen data
# =============================================================================

set.seed(1)  # for consistency

# Define factor levels and number of rows per combination
d1gen <- data.table(
    x_factor = c("A", "A", "B", "B", "C", "C"),
    x_bool = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    group_n = c(60, 40, 50, 50, 20, 70)  # unbalanced
)
# Make rows
d1 <- d1gen %>% uncount(group_n) %>% as.data.table()
d1[, subject := paste0("s", seq(1, nrow(d1)))]
# Make factors explicit
d1[, x_factor := factor(x_factor, levels = c("A", "B", "C"))]
d1[, x_bool := factor(x_bool, levels = c(FALSE, TRUE))]
# Create dependent variable
BASE_MEAN <- 5
X_SHIFT <- 2.0
A_SHIFT <- 0.0
B_SHIFT <- +1.7
C_SHIFT <- -1.7
X_YES_AND_C_INTERACTION <- 0.3
ERROR_SD <- 0.5
d1[, y :=
    BASE_MEAN
    + as.integer(x_bool) * X_SHIFT
    + ifelse(
        x_factor == "A",
        A_SHIFT,
        ifelse(x_factor == "B", B_SHIFT, C_SHIFT)
    )
    + ifelse(x_bool == TRUE & x_factor == "C", X_YES_AND_C_INTERACTION, 0)
    + rnorm(n = nrow(d1), sd = ERROR_SD)
]

# =============================================================================
# Analyse...
# =============================================================================

heading <- function(...) {
    line <- "============================================================================="
    cat(line, "\n", ..., "\n", line, "\n", sep = "")
}

heading("Type I: d1_m1, via stats::anova(lm(...))")
d1_m1 <- lm(y ~ x_bool * x_factor, data = d1)
print(stats::anova(d1_m1))
# print(summary(d1_m1))
# WOULD BE WRONG (haven't set contrasts correctly):
# drop1(d1_m1, ~., test = "F")

# CORRECT:
heading(
  "d1_m1_t3contrasts, via setting contrasts with set_type3_contrasts() then\n",
  "    lm() then drop1()\n",
  "... poor factor labelling"
)
set_type3_contrasts()
print(getOption("contrasts"))
d1_m1_t3contrasts <- lm(y ~ x_bool * x_factor, data = d1)
# print(anova(d1_m1_t3contrasts))
print(drop1(d1_m1_t3contrasts, ~., test = "F"))
print(summary(d1_m1_t3contrasts))  # ANNOYING LABELS
revert_contrasts()

# NOT THIS:
#   lmerTest::lmer(y ~ x_bool * x_factor, data = d1)
# Error: No random effects terms specified in formula

# CAN'T CHEAT:
#   lmerTest::lmer(y ~ x_bool * x_factor + (1 | subject), data = d1)
# Error: number of levels of each grouping factor must be < number of
# observations (problems: subject)

# Type II by default:
# car::Anova(d1_m1)

# WRONG Type III:
# car::Anova(d1_m1, type = "III")

# CORRECT TYPE III, with explicit rather than global contrasts:
heading(
    'Type III: d1_lm_t3contrasts, via setting contrasts explicitly, then\n',
    '    car::Anova(lm(...), type = "III"\n',
    '... poor labelling'
)
d1_lm_t3contrasts <- lm(
    y ~ x_bool * x_factor,
    data = d1,
    contrasts = list(x_bool = "contr.sum", x_factor = "contr.sum")
)
print(car::Anova(d1_lm_t3contrasts, type = "III"))
print(summary(d1_lm_t3contrasts))

# CORRECT TYPE III, with explicit but name-keeping contrasts:
heading(
    'Type III: d1_lm_t3contrasts_b, via setting contrasts explicitly\n',
    '    (via contr.sum.keepnames), then car::Anova(lm(...), type = "III"\n',
    '... better labelling'
)
d1_lm_t3contrasts_b <- lm(
    y ~ x_bool * x_factor,
    data = d1,
    contrasts = list(x_bool = "contr.sum.keepnames", x_factor = "contr.sum.keepnames")
)
print(car::Anova(d1_lm_t3contrasts_b, type = "III"))
print(summary(d1_lm_t3contrasts_b))

heading(
    'Type III: d1_lm_t3contrasts_c, similarly, but with relevel()'
)
d1[, x_bool_relevel := relevel(x_bool, ref = "FALSE")]
d1[, x_factor_relevel := relevel(x_factor, ref = "A")]
d1_lm_t3contrasts_c <- lm(
    y ~ x_bool_relevel * x_factor_relevel,
    data = d1,
    contrasts = list(x_bool_relevel = "contr.sum.keepnames", x_factor_relevel = "contr.sum.keepnames")
)
print(car::Anova(d1_lm_t3contrasts_c, type = "III"))
# NOTE: if you specify wrong (e.g. unused) factors in "contrasts", it'll be
# silently wrong.
print(summary(d1_lm_t3contrasts_c))

heading(
    'Type III: d1_lm_t3contrasts_c, similarly, but with global contrast\n',
    'settings via set_type3_contrasts_keepnames()'
)
set_type3_contrasts_keepnames()
d1_lm_t3contrasts_d <- lm(
    y ~ x_bool_relevel * x_factor_relevel,
    data = d1
)
print(car::Anova(d1_lm_t3contrasts_d, type = "III"))
# NOTE: if you specify wrong (e.g. unused) factors in "contrasts", it'll be
# silently wrong.
print(summary(d1_lm_t3contrasts_d))
revert_contrasts()

# WRONG:
# drop1(aov(d1_lm_t3contrasts), ~., type = "F")

# Good:
heading("Type III: ezANOVA(..., type = 3)")
print(ezANOVA(
    data = d1,
    dv = y,
    between = .(x_bool, x_factor),
    wid = subject,
    type = 3
))

heading("Type III: ezANOVA(..., type = 3), after relevel()")
print(ezANOVA(
    data = d1,
    dv = y,
    between = .(x_bool_relevel, x_factor_relevel),
    wid = subject,
    type = 3
))
