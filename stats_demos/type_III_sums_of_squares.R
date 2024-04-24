#!/usr/bin/Rscript
#
# type_III_sums_of_squares.R
#
# A reminder, 17 Apr 2024.
# This is oriented towards between-subjects ANOVA designs only.
# TL;DR -- see print_anova_lm_typeIIISS() helper function.
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
#
# - However, for effect sizes, the contrast aspect is also important.
#   Using "contr.sum" (or a modification) means that a two-level factor will
#   be encoded [-1, +1]. The coefficient shown is therefore HALF the effect size
#   for such a factor. The default, "contr.treatment", will code this [0, 1].
#
#   Also, by default, the "baseline" level for comparison is the FIRST for
#   treatment contrasts, and the LAST for sum contrasts.
#
#   - See e.g. https://faculty.nps.edu/sebuttre/home/r/contrasts.html.
#
# - Setting GLOBAL contrast options is in general unwise: potential for
#   mistakes. See e.g.
#
#   - https://rdoodles.rbind.io/2020/10/type-3-anova-in-r-an-easy-way-to-publish-wrong-tables/
#   - https://faculty.nps.edu/sebuttre/home/r/contrasts.html
#   - https://stat.ethz.ch/pipermail/r-help/2007-October/143047.html


# =============================================================================
# Libraries
# =============================================================================

library(data.table)  # for data.table()
library(tidyr)  # for uncount(), %>%

library(car)  # for car::Anova()
library(ez)  # for ezANOVA(), ezStats()


# =============================================================================
# Single helper function -- PREFER THIS.
# =============================================================================

print_anova_lm_typeIIISS <- function(
        formula, data, ...,
        with_effects_sum_contrasts = TRUE,
        with_effects_treatment_contrasts = TRUE) {
    # Show an ANOVA using Type III sums of squares, without affecting R's
    # global contrasts options (which may subtly mess up other work).
    #
    # Helper sub-function: better labelling of contrasts
    contr.sum.keepnames <- function(...) {
        # https://stackoverflow.com/questions/10808853/why-does-changing-contrast-type-change-row-labels-in-r-lm-summary
        conS <- contr.sum(...)
        colnames(conS) <- rownames(conS)[-length(rownames(conS))]
        conS
    }
    # Save global contrast options.
    saved_options_contrasts <- getOption("contrasts")
    # Thinking
    options(contrasts = c(unordered = "contr.sum.keepnames", ordered = "contr.poly"))
    model <- lm(formula, data = data, ...)
    anova_of_model <- car::Anova(model, type = "III")
    # Printing
    mkline <- function(char, linewidth = 79) {
        paste0(paste(rep(char, linewidth), collapse = ""), "\n")
    }
    line1 <- mkline("#")
    line2 <- mkline("-")
    line3 <- mkline("=")
    cat(line1)
    print(match.call())
    cat(line1)
    cat(line2)
    cat("ANOVA, via car::Anova(lm(...), type = 3) [Type III SS, sum-to-zero contrasts]\n")
    cat(line2)
    print(anova_of_model)
    if (with_effects_sum_contrasts) {
        cat(line2)
        cat(
            "Model summary (effects) -- note sum-to-zero contrasts and therefore effect\n",
            "sizes will NOT be as typically expected. Intercept is at the notional 'zero'\n",
            "level of all factors (not necessarily at any individual factor level).\n",
            "Reference category is the LAST level of a factor.\n",
            sep = ""
        )
        cat(line2)
        print(summary(model))
    }
    # Additional
    if (with_effects_treatment_contrasts) {
        # Back to R default contrasts (don't assume this was the default anyway!)
        options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))
        treatment_effect_model <- lm(formula, data = data, ...)
        cat(line2)
        cat(
            "Model summary (effects) -- with 'treatment' contrasts (R default). Effect\n",
            "sizes as normally expected, but not the same contrasts as the ANOVA.\n",
            "Reference category is the FIRST level of a factor.\n",
            "Intercept is at the zero (first) level of all factors.\n",
            sep = ""
        )
        cat(line2)
        print(summary(treatment_effect_model))
    }
    cat(line3)
    # Restore saved contrast options.
    options(contrasts = saved_options_contrasts)
}


# =============================================================================
# Helper functions, just for exploring
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
contr.sum.keepnames <- function(...) {
    # https://stackoverflow.com/questions/10808853/why-does-changing-contrast-type-change-row-labels-in-r-lm-summary
    conS <- contr.sum(...)
    colnames(conS) <- rownames(conS)[-length(rownames(conS))]
    conS
}
heading <- function(...) {
    line <- paste0(paste(rep("#", 79), collapse = ""), "\n")
    cat(line, ..., "\n", line, sep = "")
}


# =============================================================================
# Generate specimen data
# =============================================================================

set.seed(1)  # for consistency

mk_specimen_data <- function() {
    
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
    # ... note that this makes the numeric values 1, 2 (not 0, 1); therefore,
    #     use "as.logical" for calculations assuming 0/1.
    
    # Create dependent variable
    BASE_MEAN <- 5.0
    X_SHIFT <- 2.0
    A_SHIFT <- 0.0
    B_SHIFT <- 1.7
    C_SHIFT <- -1.7
    X_YES_AND_C_INTERACTION <- 0.3
    ERROR_SD <- 0.1
    d1[, y :=
        BASE_MEAN
        + as.integer(as.logical(x_bool)) * X_SHIFT
        + ifelse(
            x_factor == "A",
            A_SHIFT,
            ifelse(x_factor == "B", B_SHIFT, C_SHIFT)
        )
        + ifelse(x_bool == TRUE & x_factor == "C", X_YES_AND_C_INTERACTION, 0)
        + rnorm(n = nrow(d1), sd = ERROR_SD)
    ]
    # Relevel factors: the first shall be last
    d1[, x_bool_relevel := factor(x_bool, levels = c(TRUE, FALSE))]
    d1[, x_factor_relevel := factor(x_factor, levels = c("B", "C", "A"))]
    
    # Testing, x_bool only:
    d1[, y2 :=
        BASE_MEAN
        + as.integer(x_bool) * X_SHIFT
        + rnorm(n = nrow(d1), sd = ERROR_SD)
    ]
    return(d1)
}


d1 <- mk_specimen_data()

# =============================================================================
# Analyse...
# =============================================================================

if (TRUE) {
    
    heading("Type I: d1_m1, via stats::anova(lm(...))")
    d1_m1 <- lm(y ~ x_bool * x_factor, data = d1)
    print(stats::anova(d1_m1))
    # print(summary(d1_m1))
    # WOULD BE WRONG (haven't set contrasts correctly):
    # drop1(d1_m1, ~., test = "F")
    
    heading("Type I: d1_m1, via stats::anova(lm(...)) -- different factor order")
    d1_m2 <- lm(y ~ x_factor * x_bool, data = d1)
    print(stats::anova(d1_m2))
    
    # CORRECT:
    heading(
      'Type III: d1_m1_t3contrasts, via setting contrasts with\n',
      '    set_type3_contrasts() then lm() then drop1()\n',
      '... poor (numeric) labelling of coefficients for factors'
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
        '... poor (numeric) labelling of coefficients for factors'
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
    print(ezStats(
        data = d1,
        dv = y,
        between = .(x_bool, x_factor),
        wid = subject,
        type = 3
    ))
}


# =============================================================================
# Use just our helper function, for cleaner code:
# =============================================================================

print_anova_lm_typeIIISS(y ~ x_bool * x_factor, data = d1)

# print_anova_lm_typeIIISS(y2 ~ x_bool, data = d1)
