# miscresults.R
#
# -----------------------------------------------------------------------------
# Reporting standards
# -----------------------------------------------------------------------------
# Some guidelines on reporting statistics include:
# - https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics
# - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2959222/
#   ... argues for "mean (SD)" not "mean ± SD" because some will assume that
#   ± is for confidence intervals.
# - https://en.wikipedia.org/wiki/Plus%E2%80%93minus_sign
#   ... ± typically SD or SE.
# - https://en.wikipedia.org/wiki/Margin_of_error
# - Confidence intervals in JMIR style use an en dash "a–b", or "a to b" if
#   any are negative. And confidence intervals frequently do involve a negative
#   number. An alternative is [a, b], using inclusive (square) brackets.
# - Ranges typically use en dashes in writing, and less frequently involve
#   negative numbers.
# - But generally it's good to be explicit! So we'll be explicit for the
#   defaults.
#
# -----------------------------------------------------------------------------
# Markdown for flextable
# -----------------------------------------------------------------------------
# ftExtra uses Pandoc markdown;
# - https://www.uv.es/wikibase/doc/cas/pandoc_manual_2.7.3.wiki?53
# - https://ftextra.atusy.net/articles/format_columns
#
# Notably:
#
# - italics: *x*
# - bold: **x**
# - subscript: ~x~
# - superscript: ^x^
# - newline: backslash then newline immediately (see
#   miscresults$MARKDOWN_NEWLINE below)
#
# Less often:
#
# - code: `x`
# - underline: [x]{.underline}

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        car,  # for car::Anova
        flextable,
        ftExtra,  # for markup within flextable tables
        rcompanion,  # for wilcoxonZ
        rlang,  # for dots_n
        tidyverse
    )
})


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

miscresults <- new.env()


# =============================================================================
# Constants
# =============================================================================

# -----------------------------------------------------------------------------
# Unicode
# -----------------------------------------------------------------------------
# NOTE RE UNICODE: This can be challenging under Windows. The problem isn't
# usually in handling Unicode, it's in representing it in the file encoding,
# since Windows does not use UTF-8 file encoding by default. It's therefore
# preferable to use "\u<hexcode">, e.g. "\u2013" for an en dash. This is an
# ASCII file representation of a Unicode character, which works fine.

# Basic ASCII
miscresults$MARKDOWN_NEWLINE <- "\\\n"  # two characters: backslash, newline
# ... Line break: https://ftextra.atusy.net/articles/format_columns

# Common punctuation
miscresults$HYPHEN <- "-"  # plain ASCII

# Marks in the traditional order for footnotes:
# - https://en.wikipedia.org/wiki/Note_(typography)
miscresults$ASTERISK <- "*"
miscresults$DAGGER <- "\u2020"
miscresults$DOUBLE_DAGGER <- "\u2021"
miscresults$SECTION <- "\u00A7"
miscresults$DOUBLE_VERTICAL_LINE <- "\u2016"
miscresults$PILCROW <- "\u00B6"

# Dashes and mathematical symbols
miscresults$EN_DASH <- "\u2013"
miscresults$MINUS <- "\u2212"
miscresults$MULTIPLY <- "\u00D7"
miscresults$MULTIPLICATION_DOT <- "\u22C5"
miscresults$PLUS_MINUS <- "\u00B1"  # case-insensitive

# Greek letters
miscresults$ALPHA <- "\u03B1"
miscresults$CHI_LOWER <- "\u03C7"

# Accented Latin characters
miscresults$LOWER_CASE_A_ACUTE <- "\u00E1"
miscresults$UPPER_CASE_S_CARON <- "\u0160"

# Arrows and arrow-like symbols
# https://www.compart.com/en/unicode/block/U+2190
miscresults$UP_ARROW <- "\u2191"
miscresults$DOWN_ARROW <- "\u2193"
miscresults$LEFT_RIGHT_ARROW <- "\u2194"
miscresults$UP_ARROW_FROM_BAR <- "\u21A5"
miscresults$DOWN_ARROW_FROM_BAR <- "\u21A7"
# https://www.compart.com/en/unicode/block/U+25A0
miscresults$BLACK_UP_TRIANGLE <- "\u25B2"
miscresults$BLACK_DOWN_TRIANGLE <- "\u25BC"
miscresults$WHITE_UP_TRIANGLE <- "\u25B3"
miscresults$WHITE_DOWN_TRIANGLE <- "\u25BD"

# Other symbols
# https://www.compart.com/en/unicode/block/U+0080
miscresults$MIDDLE_DOT <- "\u00B7"

# Names with accents
miscresults$SIDAK_TXT <- paste0(
    miscresults$UPPER_CASE_S_CARON, "id", miscresults$LOWER_CASE_A_ACUTE, "k"
)


# -----------------------------------------------------------------------------
# Other
# -----------------------------------------------------------------------------

miscresults$DEFAULT_DP_FOR_DF <- 1  # decimal places for non-integer degrees of freedom
miscresults$MINIMUM_P_SHOWN <- 2.2e-16
    # .Machine$double.eps is 2.220446e-16; however, readers are used to seeing
    # "2.2e-16" or equivalent representations in output from R, not "2.22e-16".
miscresults$MINIMUM_F_SHOWN <- 1
miscresults$MINIMUM_ABS_T_SHOWN <- 1
    # ... critical value for a two-tailed test at α = 0.05 is ±qt(0.975, df),
    #     decreasing for higher df, and approaching qnorm(0.975) as df → ∞.
    #     So that's about 1.96. We might want to report things that didn't
    #     make it, but 1 seems like a reasonable "definitely do not care"
    #     threshold.
miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS <- TRUE
miscresults$NOT_SIGNIFICANT <- "NS"
miscresults$DEFAULT_ALPHA <- 0.05  # Per Fisher.
miscresults$DEFAULT_CI <- 1 - miscresults$DEFAULT_ALPHA

R_INTERACTION_MARKER <- stringr::fixed(":")
R_RESIDUALS_LABEL <- "Residuals"
R_INTERCEPT_LABEL <- "(Intercept)"
R_FALSE_TEXT <- "FALSE"
R_TRUE_TEXT <- "TRUE"


# =============================================================================
# Helper functions
# =============================================================================

miscresults$is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    # per example in ?base::integer
    abs(x - round(x)) < tol
}


# Can't use miscresults$ prefix here.
contr.sum.keepnames <- function(...) {
    # See type_III_sums_of_squares.R
    # https://stackoverflow.com/questions/10808853/why-does-changing-contrast-type-change-row-labels-in-r-lm-summary
    conS <- contr.sum(...)
    colnames(conS) <- rownames(conS)[-length(rownames(conS))]
    # ... For example, if the row names are A-D, this will assign the column
    # names to be A-C, dropping the last element.
    conS
}


# =============================================================================
# Formatting results: basic conversion to "pretty" text formats
# =============================================================================

miscresults$mk_sig_label <- function(
    p,
    symbol = "*",
    sidak_correction_n = 1,
    prefix = " ",
    prefix_ns = ", ",
    ns_text = miscresults$NOT_SIGNIFICANT
) {
    # From a p value, return a label, e.g. "**" or "NS", to be attached to that
    # p value (in null hypothesis significant testing).
    # Optionally, apply a Sidak correction.
    p <- miscstat$sidak_corrected_p(p, sidak_correction_n)
    return(case_when(
        p < 0.00001 ~ paste0(prefix, paste(rep(symbol, 5), collapse = "")),  # 10^-5
        p < 0.0001  ~ paste0(prefix, paste(rep(symbol, 4), collapse = "")),  # 10^-4
        p < 0.001   ~ paste0(prefix, paste(rep(symbol, 3), collapse = "")),  # 10^-3
        p < 0.01    ~ paste0(prefix, paste(rep(symbol, 2), collapse = "")),  # 10^-2
        p < 0.05    ~ paste0(prefix, paste(rep(symbol, 1), collapse = "")),  # Fisher!
        TRUE        ~ paste0(prefix_ns, ns_text)
    ))
}


miscresults$mk_p_asterisk_caption <- function(
    symbol = "*",
    ns_text = miscresults$NOT_SIGNIFICANT,
    ns_explanation = "not significant",
    suffix = "."
) {
    # Makes a helpful caption to match miscresults$mk_sig_label().
    paste0(
        paste(rep(symbol, 5), collapse = ""), " p < 0.00001; ",
        paste(rep(symbol, 4), collapse = ""), " p < 0.0001; ",
        paste(rep(symbol, 3), collapse = ""), " p < 0.001; ",
        paste(rep(symbol, 2), collapse = ""), " p < 0.01; ",
        paste(rep(symbol, 1), collapse = ""), " p < 0.05; ",
        ns_text, ", ", ns_explanation,
        suffix
    )
}


miscresults$fmt_int <- function(
    x,
    big.mark = get_flextable_defaults()$big.mark,
    use_plus = FALSE,  # prepend "+" for positive numbers?
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str
) {
    # Replaces flextable::fmt_int().
    flag <- ""
    if (use_plus) {
        flag <- paste0(flag, "+")
    }
    txt <- formatC(
        x,
        format = "d",
        flag = flag,
        big.mark = big.mark
    )
    # Convert hyphens to proper minus signs, and trim whitespace:
    txt <- stringr::str_trim(
        stringr::str_replace_all(txt, miscresults$HYPHEN, miscresults$MINUS)
    )
    # Deal with NaN and NA (in parallel):
    return(case_when(
        is.nan(x) ~ nan_str,
        is.na(x) ~ na_str,
        .default = txt
    ))
}


miscresults$fmt_float <- function(
    x,
    allow_sci_notation = TRUE,
    include_trailing_zero = TRUE,
    sf = get_flextable_defaults()$digits,
    use_plus = FALSE,  # prepend "+" for positive numbers?
    big.mark = get_flextable_defaults()$big.mark,
    decimal.mark = get_flextable_defaults()$decimal.mark,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str
) {
    # Format a floating-point (real) number, according to a number of
    # significant figures, allowing scientific notation or not. Return values
    # might look like "0.1", "2.2 × 10^−16^" (the latter using ftExtra markup).
    # An extension for flextable::fmt_dbl, using its notation.

    flag <- ""
    if (include_trailing_zero) {
        flag <- paste0(flag, "#")
    }
    if (use_plus) {
        flag <- paste0(flag, "+")
    }
    if (allow_sci_notation) {
        txt <- formatC(
            signif(x, digits = sf),
            digits = sf,
            format = "g",
            flag = flag,
            big.mark = big.mark,
            decimal.mark = decimal.mark
        )
        # May or may not be in scientific notation.
        # If scientific notation:
        scimatch <- stringr::str_match(txt, "(.+)e([-+]?)(\\d+)")
        # ... may be a matrix, indexed [item, matchgroupnum]
        # In case using scientific notation: make it pretty!
        radix <- scimatch[, 2]
        sign <- scimatch[, 3]
        exponent <- scimatch[, 4]
        sign <- stringr::str_replace(sign, "[+]", "")
        # ... remove + from exponent
        exponent <- stringr::str_replace(exponent, "^0+" ,"")
        # ... remove leading zeros from exponent
        # Use ^...^ for superscript with ftExtra.
        txt <- ifelse(
            !is.na(scimatch[, 1]),
            paste0(
                radix, " ", miscresults$MULTIPLY, " 10^", sign, exponent, "^"
            ),
            txt
        )
    } else {
        # https://stackoverflow.com/questions/3245862
        txt <- formatC(
            signif(x, digits = sf),
            digits = sf,
            format = "fg",
            flag = flag,
            big.mark = big.mark,
            decimal.mark = decimal.mark
        )
    }
    # Convert hyphens to proper minus signs, and trim whitespace:
    txt <- stringr::str_trim(
        stringr::str_replace_all(txt, miscresults$HYPHEN, miscresults$MINUS)
    )
    # Deal with NaN and NA (in parallel):
    return(case_when(
        is.nan(x) ~ nan_str,
        is.na(x) ~ na_str,
        .default = txt
    ))
}


miscresults$mk_p_text <- function(
    p,
    sf = get_flextable_defaults()$digits,
    min_p = miscresults$MINIMUM_P_SHOWN
) {
    # From a p value, return a string such as "*p* = 0.03" or "*p* < 2.2e-16".
    # Uses fmt_float().
    return(ifelse(
        p < min_p,
        paste0(
            "*p* < ",
            miscresults$fmt_float(min_p, sf = sf)
        ),
        paste0(
            "*p* = ",
            miscresults$fmt_float(p, sf = sf)
        )
    ))
}


miscresults$mk_p_text_with_label <- function(
    p,
    sf = get_flextable_defaults()$digits,
    sidak_correction_n_for_asterisks = 1,
    min_p = miscresults$MINIMUM_P_SHOWN,
    ns_text = miscresults$NOT_SIGNIFICANT
) {
    # From a p value, return an asterisk-labelled string such as "*p* = 0.03 *"
    # or "*p* = 0.5, NS". Optionally, apply a Sidak correction to the labelling
    # (without altering the p value itself).
    return(paste0(
        miscresults$mk_p_text(
            p,
            sf = sf,
            min_p = min_p
        ),
        miscresults$mk_sig_label(
            p,
            sidak_correction_n = sidak_correction_n_for_asterisks,
            ns_text = ns_text
        )
    ))
}


miscresults$mk_df_text <- function(
    df,
    dp = miscresults$DEFAULT_DP_FOR_DF,
    big.mark = "",
    decimal.mark = get_flextable_defaults()$decimal.mark
) {
    # Format degrees of freedom (df) appropriately -- as an exact integer, or
    # to 1 dp if it is not integer (since knowing the fact of not being an
    # integer is often quite important!).
    # - By default, big.mark is "", not get_flextable_defaults()$big.mark;
    #   commas in degrees of freedom would be very confusing for F tests, in
    #   which we will separate the two df numbers by commas anyway.
    # - So for consistency, we'll use "" as the default.
    return(ifelse(
        as.integer(df) == df,
        miscresults$fmt_int(df, big.mark = big.mark),  # integer version
        formatC(
            df,
            format = "f",
            digits = dp,
            big.mark = big.mark,
            decimal.mark = decimal.mark
        )  # floating-point version
    ))
}


miscresults$fmt_pct <- function(
    proportion,
    include_trailing_zero = FALSE,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str
) {
    # Format a proportion (e.g. 0.5) as a percentage (e.g. "50%").
    pct <- 100 * proportion
    txt <- paste0(
        miscresults$fmt_float(
            pct,
            allow_sci_notation = FALSE,
            include_trailing_zero = include_trailing_zero
        ),
        "%"
    )
    return(case_when(
        is.nan(proportion) ~ nan_str,
        is.na(proportion) ~ na_str,
        .default = txt
    ))
}

miscresults$fmt_n_percent <- function(
    n,
    proportion,
    na_str = get_flextable_defaults()$na_str,
    ...
) {
    # Format a number and a proportion as "n (x%)", where x is the percentage
    # form of the proportion. As for flextable::fmt_n_percent, but emphasizes
    # significant figures rather than decimal places for the percentage.
    #
    # Additional parameters are passed to fmt_pct().
    ifelse(
        is.na(n) | is.na(proportion),
        na_str,
        paste0(
            miscresults$fmt_int(n),
            " (",
            miscresults$fmt_pct(proportion, ...),
            ")"
        )
    )
}


miscresults$mk_n_percent <- function(
    n,
    total,
    min_threshold = NA,
    less_than_symbol = "< ",
    ...
) {
    # Format a number as "n (x%)", where x is the percentage form of n / total.
    # - min_threshold: if specified (not NA), numbers below this are replaced
    #   by "<[threshold]", and similarly for percentages (small number
    #   suppression)
    # - Additional parameters are passed to fmt_n_percent().
    proportion <- n / total
    return(ifelse(
        !is.na(min_threshold) & n < min_threshold,
        paste0(
            less_than_symbol,
            miscresults$fmt_int(min_threshold),
            " (",
            less_than_symbol,
            miscresults$fmt_pct(min_threshold / total, ...),
            ")"
        ),  # with small-number suppression
        miscresults$fmt_n_percent(n, proportion, ...)  # unsuppressed
    ))
}


miscresults$fmt_n_percent_low_high <- function(
    n_low, n_high, total,
    range_sep = " to ",
    # ... an en dash is feasible but does break when using scientific notation
    # and is a little confusing if there are also minus signs.
    na_str = get_flextable_defaults()$na_str,
    ...
) {
    # Format a number as "n_low-n_high (x-y%)": a "vague" version of
    # mk_n_percent that can be used for small-number suppression for totals.
    # For example, if you have group sizes A=50, B=50, C=<10, then the total is
    # 100-109.
    # - The percentages are n_low/total and n_high/total.
    # - Additional parameters are passed to fmt_pct().
    prop_low <- n_low / total
    prop_high <- n_high / total
    ifelse(
        is.na(n_low) | is.na(n_high) | is.na(prop_low) | is.na(prop_high),
        na_str,
        paste0(
            miscresults$fmt_int(n_low),
            range_sep,
            miscresults$fmt_int(n_high),
            " (",
            miscresults$fmt_pct(prop_low, ...),
            range_sep,
            miscresults$fmt_pct(prop_high, ...),
            ")"
        )
    )
}


miscresults$fmt_value_sd <- function(
    x,
    sigma,
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE,
    value_prefix = "",
    # sd_prefix = paste0(" (", miscresults$PLUS_MINUS, " "),
    sd_prefix = " (SD ",
    sd_suffix = ")",
    na_str = get_flextable_defaults()$na_str
) {
    # Given a value x and a standard deviation sigma, show this as "x (± σ)",
    # or similar.
    # See also flextable::fmt_avg_dev(avg, dev), which is less flexible.
    x_text <- miscresults$fmt_float(
        x, sf = sf, allow_sci_notation = allow_sci_notation
    )
    s_text <- miscresults$fmt_float(
        sigma, sf = sf, allow_sci_notation = allow_sci_notation
    )
    results <- paste0(value_prefix, x_text, sd_prefix, s_text, sd_suffix)
    return(ifelse(is.na(x), na_str, results))
}


miscresults$mk_mean_sd <- function(
    x,
    na.rm = TRUE,
    ...
) {
    # Calculate a mean and standard deviation (SD) from the vector provided, and
    # show this as "μ (± σ)", or similar. Additional parameters are passed to
    # fmt_value_sd().
    return(miscresults$fmt_value_sd(
        x = mean(x, na.rm = na.rm),
        sigma = sd(x, na.rm = na.rm),
        ...
    ))
}


miscresults$fmt_value_ci <- function(
    x,
    ci_lower,
    ci_upper,
    value_prefix = "",
    ci_prefix = " (CI ",
    ci_sep = " to ",  # miscresults$EN_DASH also possible
    ci_suffix = ")",
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE,
    na_str = get_flextable_defaults()$na_str,
    ...  # passed to fmt_float
) {
    # Given a value x and confidence interval limits ci_lower, ci_upper, show
    # this as e.g. "x (a–b)".
    results <- paste0(
        value_prefix,
        miscresults$fmt_float(
            x,
            sf = sf, allow_sci_notation = allow_sci_notation, ...
        ),
        ci_prefix,
        miscresults$fmt_float(
            ci_lower,
            sf = sf, allow_sci_notation = allow_sci_notation, ...
        ),
        ci_sep,
        miscresults$fmt_float(
            ci_upper,
            sf = sf, allow_sci_notation = allow_sci_notation, ...
        ),
        ci_suffix
    )
    return(ifelse(is.na(x), na_str, results))
}


miscresults$mk_mean_ci <- function(
    x,
    ci = miscresults$DEFAULT_CI,
    na.rm = TRUE,
    ...
) {
    # From a vector, show a mean and confidence interval (e.g. 95% CI) as e.g.
    # "μ (a–b)". Additional parameters are passed to fmt_value_ci().
    mu <- mean(x, na.rm = na.rm)
    ci_pair <- miscstat$confidence_interval_t(x, ci = ci, na.rm = na.rm)
    ci_lower <- ci_pair["ci_lower"]
    ci_upper <- ci_pair["ci_upper"]
    return(miscresults$fmt_value_ci(
        x = mu,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        ...
    ))
}


miscresults$fmt_median_range <- function(
    med,
    range_lower,
    range_upper,
    median_prefix = "median ",
    range_prefix = " (range ",
    range_sep = " to ",
    # ... an en dash is feasible but does break when using scientific notation
    # and is a little confusing if there are also minus signs.
    range_suffix = ")",
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE,
    na_str = get_flextable_defaults()$na_str
) {
    # Given a median and range limits, this as e.g. "m (a–b)".
    # Remember to think in parallel.
    all_three_integer <- (
        miscresults$is.wholenumber(range_lower)
        & miscresults$is.wholenumber(range_lower)
        & miscresults$is.wholenumber(range_upper)
    )
    formatter <- function(x) {
        ifelse(
            all_three_integer,
            miscresults$fmt_int(x),
            miscresults$fmt_float(
                x,
                sf = sf, allow_sci_notation = allow_sci_notation
            )
        )
    }
    median_text <- formatter(med)
    range_lower_text <- formatter(range_lower)
    range_upper_text <- formatter(range_upper)
    results <- paste0(
        median_prefix,
        median_text,
        range_prefix,
        range_lower_text,
        range_sep,
        range_upper_text,
        range_suffix
    )
    return(ifelse(is.na(med), na_str, results))
}


miscresults$mk_median_range <- function(
    x,
    na.rm = TRUE,
    ...
) {
    # From a vector, show a median and range as e.g. "m (a–b)". Additional
    # parameters are passed to fmt_median_range().
    med <- median(x, na.rm = na.rm)
    range_lower <- min(x, na.rm = na.rm)
    range_upper <- max(x, na.rm = na.rm)
    return(miscresults$fmt_median_range(
        med = med,
        range_lower = range_lower,
        range_upper = range_upper,
        ...
    ))
}


miscresults$fmt_chisq <- function(
    chisq, df,
    min_chisq = 1  # minimum value shown exactly
    # ... critical value is qchisq(0.95, df) at α=0.05 and df=1, increasing
    #     for higher df. We might want to report things that didn't
    #     make it, but 1 seems like a reasonable "definitely do not care"
    #     threshold.
) {
    # Format a chi-square statistic (without a p value).
    # - Note that chi-square can only be positive.
    df_txt <- miscresults$mk_df_text(df)
    symbol_df_txt <- sprintf(
        paste0("*", miscresults$CHI_LOWER, "*^2^~%s~"),
        df_txt
    )
    return(ifelse(
        chisq < min_chisq,
        sprintf("%s < %s", symbol_df_txt, min_chisq),
        sprintf("%s = %s", symbol_df_txt, miscresults$fmt_float(chisq))
    ))
}


miscresults$fmt_chisq_p <- function(
    chisq, df, p,
    min_chisq = 1,  # see miscresults$fmt_chisq()
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = DEFAULT_ALPHA
) {
    # Format a chi-square statistic with a p value).
    chisq_txt <- miscresults$fmt_chisq(chisq, df, min_chisq = min_chisq)
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    if (any(chisq < min_chisq & p < check_alpha)) {
        stop(sprintf(
            "chisq (%f) < %f but p = %f so disallowing visual shortcut",
            chisq, min_chisq, p
        ))
    }
    return(ifelse(
        chisq < min_chisq,
        sprintf("%s, %s", chisq_txt, ns_text),
        sprintf("%s, %s", chisq_txt, p_txt)
    ))
}


miscresults$fmt_t <- function(
    t, df,
    min_abs_t = miscresults$MINIMUM_ABS_T_SHOWN,
    omit_df_below_min_t = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS
) {
    # Format a t statistic (without a p value).
    # - Note that t can be negative or positive.
    df_txt <- ifelse(
        is.na(df),
        "",
        sprintf("~%s~", miscresults$mk_df_text(df))
    )
    t_txt <- miscresults$fmt_float(t, use_plus = TRUE)
    return(ifelse(
        abs(t) < min_abs_t,
        ifelse(
            omit_df_below_min_t,
            sprintf("|*t*| < %s", min_abs_t),
            sprintf("|*t*%s| < %s", df_txt, min_abs_t)
        ),
        sprintf("*t*%s = %s", df_txt, t_txt)
    ))
}


miscresults$fmt_t_p <- function(
    t, df, p,
    min_abs_t = miscresults$MINIMUM_ABS_T_SHOWN,
    omit_df_below_min_t = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = DEFAULT_ALPHA
) {
    # Format a t statistic with a p value.
    t_txt <- miscresults$fmt_t(
        t, df,
        min_abs_t = min_abs_t,
        omit_df_below_min_t = omit_df_below_min_t
    )
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    if (any(abs(t) < min_abs_t & p < check_alpha)) {
        stop(sprintf(
            "|t| (|%f|) < %f but p = %f so disallowing visual shortcut",
            t, min_abs_t, p
        ))
    }
    return(ifelse(
        abs(t) < min_abs_t,
        sprintf("%s, %s", t_txt, ns_text),
        sprintf("%s, %s", t_txt, p_txt)
    ))
}


miscresults$fmt_F <- function(
    F, df1, df2,
    min_F = miscresults$MINIMUM_F_SHOWN,
    omit_df_below_min_F = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS
) {
    # Format an F statistic (without a p value).
    # - Note that F will always be positive.
    # - No commas in degrees of freedom (big.mark = ""), since they are
    #   separated by commas anyway.
    df1_txt <- miscresults$mk_df_text(df1, big.mark = "")
    df2_txt <- miscresults$mk_df_text(df2, big.mark = "")
    df_txt <- sprintf("~%s,%s~", df1_txt, df2_txt)
    return(ifelse(
        F < min_F,
        ifelse(
            omit_df_below_min_F,
            sprintf("*F* < %s", min_F),
            sprintf("*F*%s < %s", df_txt, min_F)
        ),
        sprintf("*F*%s = %s", df_txt, miscresults$fmt_float(F))
    ))
}


miscresults$fmt_F_p <- function(
    F, df1, df2, p,
    min_F = miscresults$MINIMUM_F_SHOWN,
    omit_df_below_min_F = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = DEFAULT_ALPHA
) {
    # Format an F statistic with a p value.
    f_txt <- miscresults$fmt_F(
        F, df1, df2,
        min_F = min_F,
        omit_df_below_min_F = omit_df_below_min_F
    )
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    if (any(F < min_F & p < check_alpha)) {
        stop(sprintf(
            "F (%f) < %f but p = %f so disallowing visual shortcut",
            F, min_F, p
        ))
    }
    return(ifelse(
        F < min_F,
        sprintf("%s, %s", f_txt, ns_text),  # e.g. "F[df1,df2] < 1, NS"
        sprintf("%s, %s", f_txt, p_txt)  # e.g. "F[df1,df2] = 5, p = ..."
    ))
}


miscresults$fmt_Z <- function(Z, use_plus = TRUE) {
    # Format a Z statistic (without a p value).
    z_txt <- miscresults$fmt_float(Z, use_plus = use_plus)
    return(sprintf("*Z* = %s", z_txt))
}


miscresults$fmt_Z_p <- function(
    Z, p,
    use_plus = TRUE,
    ns_text = miscresults$NOT_SIGNIFICANT
) {
    # Format a Z statistic with a p value.
    z_txt <- miscresults$fmt_Z(Z, use_plus = use_plus)
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    return(sprintf("%s, %s", z_txt, p_txt))
}


miscresults$fmt_predictor <- function(
    predictor_txt,
    replacements = NULL,  # e.g. c("from1" = "to1", "from1" = "to2", ...)
    interaction_txt = paste0(" ", miscresults$MULTIPLY, " ")
) {
    # Format a predictor nicely, e.g. changing "drug:sex" to "Drug x Sex".
    predictor_txt <- stringr::str_replace_all(
        predictor_txt,
        pattern = R_INTERACTION_MARKER,
        replacement = interaction_txt
    )
    if (!is.null(replacements)) {
        predictor_txt <- stringr::str_replace_all(
            predictor_txt,
            pattern = replacements
        )
    }
    return(predictor_txt)
}


miscresults$fmt_single_level <- function(
    level_txt,
    anova_term_txt,
    replacements = NULL,
    interaction_txt = paste0(" ", miscresults$MULTIPLY, " "),
    remove_blanks = TRUE
) {
    # Similar to fmt_predictor(), but for formatting levels; for example,
    # converting "drugLowDose:sexMale" to "Low dose x Male".
    #
    # Arguments:
    #   level_txt
    #       Name of the level, e.g. "drugLowDose:sexMale".
    #   anova_term_txt
    #       Name of the ANOVA term, e.g. "drug:sex".
    #   replacements
    #       Optional vector of the form c("from1" = "to1", "from1" = "to2",
    #       ...), with which to replace text.
    #   interaction_txt
    #       Text to use to join components of the level, e.g. " x " or ", ".
    #   remove_blanks
    #       Blank components may be generated from continuous predictors.
    #       If remove_blanks == TRUE, these will be removed.
    #
    # Returns:
    #   The name of the level, with components joined by interaction_txt.
    #
    # See fmt_level() for a vectorized version.

    if (is.na(level_txt) || level_txt == R_INTERCEPT_LABEL) {
        return(level_txt)
    }
    level_n_parts <- stringr::str_count(level_txt, R_INTERACTION_MARKER) + 1
    anova_n_parts <- stringr::str_count(anova_term_txt, R_INTERACTION_MARKER) + 1
    stopifnot(level_n_parts == anova_n_parts)
    getParts <- function(x) {
        stringr::str_split_1(x, R_INTERACTION_MARKER)
    }
    level_parts <- getParts(level_txt)
    anova_parts <- getParts(anova_term_txt)
    result_parts <- str_remove(
        level_parts,
        pattern = stringr::fixed(anova_parts)
    )
    if (!is.null(replacements)) {
        result_parts <- stringr::str_replace_all(
            result_parts,
            pattern = replacements
        )
    }
    if (remove_blanks) {
        result_parts <- result_parts[!is.na(result_parts) & result_parts != ""]
    }
    return(stringr::str_c(result_parts, collapse = interaction_txt))
}


miscresults$fmt_level <- Vectorize(
    miscresults$fmt_single_level,
    vectorize.args = c("level_txt", "anova_term_txt")
)
# See miscresults$fmt_single_level(), but vectorized.


# =============================================================================
# Formatted statistical tests
# =============================================================================

miscresults$mk_chisq_contingency <- function(
    x_counts,
    y_counts = NULL,
    p = rep(1 / length(x_counts), length(x_counts)),
    min_chisq = 1,  # see miscresults$fmt_chisq()
    ns_text = miscresults$NOT_SIGNIFICANT,
    debug = FALSE,
    ...
) {
    # Reports chi-squared to 1 dp.
    # Both x_counts and y_counts should be vectors of integers. (They are not
    # named x and y because of the differing syntax of chisq.test for x-and-y
    # rather than the contingency table/matrix form.) The alternative is to
    # specify p (expected probabilities) instead of y_counts.
    # Any additional parameters are passed to chisq.test().

    # -------------------------------------------------------------------------
    # TESTS:
    #
    # - p75 of RNC's 2004 stats handout:
    # mk_chisq_contingency(x_counts = c(153, 105), y_counts = c(24, 76), correct = FALSE)
    # ... gives chisq = 35.93, df = 1 as expected, without continuity
    #     correction.
    # ... with correct = TRUE (the default), a slightly different answer.
    #
    # - p84, Q4 (answers p101):
    # mk_chisq_contingency(c(53, 48, 75, 49, 60, 57), correct = FALSE)
    # ... gives chisq = 8.67, df = 5 as expected.
    # -------------------------------------------------------------------------

    # Check parameters
    if (!all(x_counts == floor(x_counts))) {
        stop("x_counts should contain only integers")
    }
    if (!is.null(y_counts)) {
        # using y_counts
        stopifnot(length(x_counts) == length(y_counts))
        if (!all(y_counts == floor(y_counts))) {
            stop("y_counts should contain only integers")
        }
    } else {
        # using p
        stopifnot(length(x_counts) == length(p))
    }

    # Perform chi-square test
    if (!is.null(y_counts)) {
        # d <- matrix(c(x_counts, y_counts), byrow = FALSE, ncol = 2)
        d <- as.matrix(data.frame(x = x_counts, y = y_counts))
        # Using as.matrix() is safer! The plain matrix version shown above is
        # correct, but the danger is of mapping it wrong (with byrow, ncol,
        # etc.).
        if (debug) {
            print(d)
        }
        result <- chisq.test(x = d, ...)
    } else {
        if (debug) {
            print(x_counts)
            print(p)
        }
        result <- chisq.test(x = x_counts, p = p, ...)
    }
    if (debug) {
        print(result)
    }

    # Extract and format results
    return(miscresults$fmt_chisq_p(
        chisq = result$statistic,
        df = result$parameter,
        p = result$p.value,
        min_chisq = min_chisq,
        ns_text = ns_text
    ))
}


miscresults$mk_t_test <- function(
    x,
    y = NULL,
    min_abs_t = miscresults$MINIMUM_ABS_T_SHOWN,
    omit_df_below_min_t = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = DEFAULT_ALPHA,
    debug = FALSE,
    ...
) {
    # Reports a t test. Any additional parameters are passed to t.test().
    # The direction of t is "x - y" (i.e. positive if x > y, negative if x < y).
    result <- t.test(x = x, y = y, ...)
    if (debug) {
        print(result)
    }
    return(miscresults$fmt_t_p(
        t = result$statistic,
        df = result$parameter,
        p = result$p.value,
        min_abs_t = min_abs_t,
        omit_df_below_min_t = omit_df_below_min_t,
        ns_text = ns_text,
        check_alpha = check_alpha
    ))
}


miscresults$mk_wilcoxon_test <- function(
    x,
    y = NULL,
    ns_text = miscresults$NOT_SIGNIFICANT,
    debug = FALSE,
    ...
) {
    # Reports a Wilcoxon one- or two-sample test; the two-sample test is the
    # Mann-Whitney U test, which is the same as the Wilcoxon rank-sum test.
    # Follows argument convention for wilcox.test(); any additional parameters
    # are passed to that, and likewise to rcompanion::wilcoxonZ().
    # The direction of Z is "x - y" (i.e. positive if x > y, negative if x < y).
    # See also:
    # - https://www.researchgate.net/post/How_do_I_report_a_Two-sample_Wilcoxon_Test
    result <- wilcox.test(x = x, y = y, ...)
    w <- result$statistic  # can be non-integer
    z <- rcompanion::wilcoxonZ(x = x, y = y, ...)
    if (debug) {
        print(result)
        print(z)
    }
    w_txt <- miscresults$fmt_float(w)
    # There is no df parameter here; result$parameter will be NULL.
    p <- result$p.value
    z_p_txt <- miscresults$fmt_Z_p(z, p, ns_text = ns_text)
    return(sprintf("*W* = %s, %s", w_txt, z_p_txt))
}


miscresults$mk_oneway_anova <- function(
    depvar,
    factorvar,
    debug = FALSE,
    ...
) {
    # Reports a one-way ANOVA predicting depvar by factorvar.
    # - Note that for one-way ANOVA, the sum of squares "type" is not
    #   applicable, since there is only one predictor.
    # - Additional parameters go to fmt_F_p().
    d <- data.frame(dv = depvar, x = factorvar)
    a <- aov(dv ~ x, data = d)
    s <- summary(a)
    stopifnot(length(s) == 1)
    s1 <- s[[1]]
    F <- s1$`F value`[1]
    df <- s1$Df
    stopifnot(length(df) == 2)
    df_num <- df[1]
    df_denom <- df[2]
    p <- s1$`Pr(>F)`[1]
    if (debug) {
        cat("---\n")
        print(s)
        print(F)
        print(df_num)
        print(df_denom)
        print(p)
        cat("---\n")
    }
    return(miscresults$fmt_F_p(F, df_num, df_denom, p, ...))
}


# =============================================================================
# Formatting full GLM/ANOVA models
# =============================================================================

# -----------------------------------------------------------------------------
# Internal functions
# -----------------------------------------------------------------------------

miscresults$mk_default_flextable_from_markdown <- function(markdown_table) {
    return(
        markdown_table
        %>% flextable()
        %>% ftExtra::colformat_md(part = "all")
        # ... need part = "all" to affect header and body
        %>% flextable::valign(valign = "top")  # align all cells top
        %>% flextable::autofit()  # size columns
    )
}


miscresults$which_anova_term_matches_coeff <- function(
    anova_terms,
    coeff_term,
    debug = FALSE
) {
    # Arguments:
    #
    #   anova_terms
    #       Vector of ANOVA terms, using R's standard interaction marker (":");
    #       e.g. c("x", "xx", "x:xx").
    #   coeff_term
    #       Text, e.g. "xxB" or "xB:xxB".
    #
    # Returns the index of anova_terms for which coeff_term is the best match.

    stopifnot(length(coeff_term) == 1)  # not parallel yet!
    if (debug) {
        cat("which_anova_term_matches_coeff:\n")
        cat("... anova_terms:\n")
        print(anova_terms)
        cat("... coeff_term:\n")
        print(coeff_term)
    }

    # Exact match preferred
    if (coeff_term %in% anova_terms) {
        # Coefficient term exactly matches ANOVA term.
        # Usually implies a linear predictor, not a factor.
        if (debug) {
            cat("... exact match.\n")
        }
        return(which(anova_terms == coeff_term))
    }

    # Otherwise, we are going to match e.g.
    #       drugLowDose -> drug
    #       drugLowDose:sexMale -> drug:sex
    getParts <- function(x) {
        stringr::str_split_1(x, R_INTERACTION_MARKER)
    }
    termtable <- (
        tibble(anova_term = anova_terms)
        %>% mutate(
            term_idx = row_number(),
            anova_n_parts = stringr::str_count(
                anova_term, R_INTERACTION_MARKER
            ) + 1
        )
        %>% arrange(desc(anova_term))
        # Sort by reverse alphabetical order. This will put e.g. "xx" before
        # "x", so we match the longest possible string as we proceed in
        # conventional order through the table.
    )
    coeff_n_parts <- stringr::str_count(coeff_term, R_INTERACTION_MARKER) + 1
    coeff_parts <- getParts(coeff_term)
    if (debug) {
        cat("... termtable:\n")
        print(termtable)
        cat("... coeff_n_parts:\n")
        print(coeff_n_parts)
        cat("... coeff_parts:\n")
        print(coeff_parts)
    }
    for (i in 1:nrow(termtable)) {
        arow <- termtable[i, ]
        if (arow$anova_n_parts != coeff_n_parts) {
            # Wrong number of parts
            next
        }
        anova_parts <- getParts(arow$anova_term)
        ok <- TRUE
        for (j in 1:coeff_n_parts) {
            part_ok <- stringr::str_starts(
                coeff_parts[j],
                stringr::fixed(anova_parts[j])
            )
            if (!part_ok) {
                ok <- FALSE
                break
            }
        }
        if (ok) {
            matching_term_idx <- arow$term_idx
            if (debug) {
                cat("... returning ", matching_term_idx, "\n", sep = "")
            }
            return(matching_term_idx)
        }
    }

    # Failure.
    return(NA_integer_)
}

# if (FALSE) {
#     # TESTS:
#     # ... particularly when one factor name starts with another factor's name!
#     tmp_anova_terms1 <- c("x", "xx", "x:xx")
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms1, "x") == 1)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms1, "xx") == 2)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms1, "xB") == 1)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms1, "xxB") == 2)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms1, "xB:xxB") == 3)
#     tmp_anova_terms2 <- c("xx", "x", "xx:x")
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms2, "xx") == 1)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms2, "x") == 2)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms2, "xxB") == 1)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms2, "xB") == 2)
#     stopifnot(which_anova_term_matches_coeff(tmp_anova_terms2, "xxB:xB") == 3)
# }


miscresults$summarize_anova_table <- function(a) {
    # INTERNAL FUNCTION.
    # Coerces an ANOVA table to a standard internal format.
    #
    # Various kinds of model, and different ANOVA summarizing functions, create
    # slightly different kinds of table formats, so we'll try to unify them
    # here (or raise an error if we don't know how).
    #
    # Arguments:
    #   a
    #       Output of e.g. stats::anova(some_model), or car::Anova(some_model,
    #       ...).
    #
    # Returns a table (data frame) with the following columns:
    #   term
    #       Term name.
    #   is_term:
    #       Logical: is this a subterm? Always TRUE, but provided for
    #       integration with the output of
    #       miscresults$summarize_model_coefficients().
    #   term_idx:
    #       Row number (term index).
    #   is_subterm
    #       Logical: is this a subterm? Always FALSE, but provided for
    #       integration with the output of
    #       miscresults$summarize_model_coefficients().
    #   subterm_idx:
    #       Always 0. For integration as above.
    #   is_intercept
    #       Logical: is this the main intercept term?
    #   F
    #       Value of F.
    #   df
    #       Term (numerator) degrees of freedom for the F test.
    #   df_resid
    #       Residual (denominator) degrees of freedom for the F test.
    #   pF
    #       Probability associated with F[df, df_resid].

    nrow_anova <- nrow(a)

    if (identical(colnames(a), c("Sum Sq", "Df", "F value", "Pr(>F)"))
            && rownames(a)[nrow_anova] == R_RESIDUALS_LABEL) {
        # Table looks like this:
        #
        # Anova Table (Type III tests)      # e.g. from car::Anova()
        # [or]
        # Analysis of Variance Table        # e.g. from stats::anova()
        # [but either way:]
        #
        # Response: performance
        #                       Sum Sq  Df    F value    Pr(>F)
        # (Intercept)           321575   1 80848.8063 < 2.2e-16 ***
        # age                     1937   1   486.9119 < 2.2e-16 ***
        # drug                     647   2    81.3622 < 2.2e-16 ***
        # ...
        # age:drug                  10   2     1.2526 0.2865443
        # ...
        # Residuals               2291 576

        anova_table <- (
            a
            %>% as_tibble()
            %>% select(c("Df", "F value", "Pr(>F)"))
            %>% rename(
                F = "F value",
                df = "Df",
                pF = "Pr(>F)"
            )
            %>% mutate(
                term = rownames(a),
                is_subterm = FALSE,
                df_resid = a$Df[nrow_anova],
                is_intercept = term == R_INTERCEPT_LABEL
            )
            %>% filter(term != R_RESIDUALS_LABEL)
        )

    } else if (identical(colnames(a), c("Sum Sq", "Df", "F values", "Pr(>F)"))
            && rownames(a)[nrow_anova] == R_RESIDUALS_LABEL) {
        # Table looks like this:
        #
        # Analysis of Deviance Table (Type III tests)
        #
        # Response: succeeded
        # Error estimate based on Pearson residuals
        #
        #           Sum Sq  Df F values    Pr(>F)
        # age       100.83   1 157.2595 < 2.2e-16 ***
        # drug      458.74   2 357.7439 < 2.2e-16 ***
        # sex        35.81   1  55.8508 2.819e-13 ***
        # drug:sex    2.51   2   1.9606    0.1417
        # Residuals 380.20 593

        anova_table <- (
            a
            %>% as_tibble()
            %>% select(c("Df", "F values", "Pr(>F)"))
            %>% rename(
                F = "F values",
                df = "Df",
                pF = "Pr(>F)"
            )
            %>% mutate(
                term = rownames(a),
                df_resid = a$Df[nrow_anova]
            )
            %>% filter(term != R_RESIDUALS_LABEL)
        )

    } else if (identical(colnames(a), c("Sum Sq", "Mean Sq", "Num DF",
                                        "DenDF", "F value", "Pr(>F)"))) {
        # Table looks like this, e.g. stats::anova(lmerTest::lmer(...))
        #
        # Type III Analysis of Variance Table with Satterthwaite's method
        #                       Sum Sq Mean Sq NumDF   DenDF   F value    Pr(>F)
        # age                   9168.9  9168.9     1  575.99 2325.2386 < 2.2e-16 ***
        # drug                  1734.9   867.4     2  575.99  219.9834 < 2.2e-16 ***
        # ...
        # age:drug                16.0     8.0     2  575.99    2.0320   0.13201
        # ...
        # age:drug:sex:boolpred   13.9     7.0     2  575.99    1.7632   0.17241

        anova_table <- (
            a
            %>% as_tibble()
            %>% select(c("NumDF", "DenDF", "F value", "Pr(>F)"))
            %>% rename(
                F = "F value",
                df = "NumDF",
                df_resid = "DenDF",
                pF = "Pr(>F)"
            )
            %>% mutate(
                term = rownames(a)
            )
        )

    } else if (identical(colnames(a), c("F", "Df", "Df.res", "Pr(>F)"))) {
        # Table looks like this:
        #
        # Analysis of Deviance Table (Type III Wald F tests with Kenward-Roger df)
        #
        # Response: performance
        #                                F Df  Df.res    Pr(>F)
        # (Intercept)           2.5122e+05  1  908.14 < 2.2e-16 ***
        # age                   2.3252e+03  1  576.00 < 2.2e-16 ***
        # drug                  2.1998e+02  2  576.00 < 2.2e-16 ***
        # ...
        # age:drug:sex:boolpred 1.7632e+00  2  576.00   0.17241

        anova_table <- (
            a
            %>% as_tibble()
            # No extraneous columns.
            %>% rename(
                # F is already correct.
                df = "Df",
                df_resid = "Df.res",
                pF = "Pr(>F)"
            )
            %>% mutate(
                term = rownames(a)
            )
        )

    } else {
        cat("~~~ ANOVA table, from ANOVA model:\n")
        print(a)
        stop(paste0(
            "This function can't yet extract details from this type of ",
            "ANOVA table."
        ))
    }

    # Add some final information and choose sensible column order (cosmetic).
    return(
        anova_table
        %>% mutate(
            term_idx = 1:nrow(anova_table),
            is_term = TRUE,
            is_subterm = FALSE,
            subterm_idx = 0,
            is_intercept = term == R_INTERCEPT_LABEL
        )
        %>% select(
            term,
            is_term,
            term_idx,
            is_subterm,
            subterm_idx,
            is_intercept,
            F,
            df,
            df_resid,
            pF
        )
    )
}


miscresults$summarize_model_coefficients <- function(
    m,
    anova_table,
    include_reference_levels = TRUE,
    ci = miscresults$DEFAULT_CI
) {
    # INTERNAL FUNCTION.
    # Extracts coefficients from a model in a standard internal format.
    #
    # Arguments:
    #   m
    #       Some kind of linear model, or similar.
    #   anova_table
    #       Our internal ANOVA table representation, for matching term names.
    #   include_reference_levels
    #       Include reference levels of factors (though without coefficient
    #       detail, of course).
    #   ci
    #       Which confidence intervals to use? Default is 0.95, meaning 95%
    #       confidence intervals.
    #
    # Returns a list with the following elements:
    #   coeff_summary
    #       Original version of summary(m).
    #   using_t_not_Z
    #       Coefficient tests are t tests (not Z tests). If FALSE, they're
    #       Z tests.
    #   coeff_detail
    #       Table (data frame) with the following columns:
    #           coeff_name
    #               Coefficient name, e.g. level of a factor. (May sometimes
    #               be the same as an ANOVA term name, but not always.)
    #           term_idx
    #               Index (row number in anova_table) of the corresponding
    #               ANOVA term, e.g. the factor of which this is one level, or
    #               similar. May also be 0 in the case of an intercept term
    #               when none is explicitly present in the ANOVA table.
    #           anova_term_name
    #               Corresponding term name from the ANOVA table.
    #           is_term
    #               Always FALSE; provided for integration with the output of
    #               miscresults$summarize_anova_table().
    #           is_subterm
    #               Always TRUE (as coefficients are considered "subterms" of
    #               their ANOVA term). Provided for integration with the output
    #               of miscresults$summarize_anova_table().
    #           subterm_idx:
    #               Index of this subterm, within the corresponding term.
    #               Usually numbered from 1 upwards (compare 0 for the main
    #               term in the ANOVA table, as above), but 0.5 for "reference"
    #               levels if included, to fit them in conceptually between the
    #               term heading and the first "real" level (with a
    #               coefficient).
    #           is_intercept
    #               Logical: is this the model's main intercept?
    #           is_linear
    #               Logical: is this a linear (continuous) predictor?
    #           is_reference_level
    #               Logical: is this a reference level, i.e. a dummy row
    #               representing a reference level of a factor (against which
    #               other levels are compared), with no coefficient of its own?
    #           coeff
    #               The value of the coefficient.
    #           se
    #               The standard error of the coefficient.
    #           ci_lower
    #               Lower bound of the confidence interval for the coefficient.
    #           ci_upper
    #               Upper bound of the confidence interval for the coefficient.
    #           using_t_not_Z
    #               A copy of the using_t_not_Z variable, this one present in
    #               every table row. Tells you what coeff_stat is.
    #           coeff_stat
    #               The statistic (t or Z; see using_t_not_Z) associated with
    #               the coefficient.
    #           p_coeff_stat
    #               The p value associated with the test statistic.
    #           coeff_df_for_t
    #               If the test statistic is t, the associated degrees of
    #               freedom. (NA for Z tests.)

    # Core data
    s <- summary(m)
    # ... of class e.g. summary.lm, and of mode list
    coeffs <- s$coefficients
    # ... of classes: matrix, array
    # For some summaries of models from glm(), coefficient headings may be
    #       Estimate, Std. Error, z value, Pr(>|z|)
    # For others, e.g. from lm(), may be
    #       Estimate, Std. Error, t value, Pr(>|t|)

    # Convenience variables
    coeff_colnames <- colnames(coeffs)
    coeff_rownames <- rownames(coeffs)
    n_anova_terms <- nrow(anova_table)

    # Work out if we will use t or Z tests:
    # For t tests, also work out where we'll get the df information from.
    using_t_not_Z <- "t value" %in% coeff_colnames
    if (!using_t_not_Z && !("z value" %in% coeff_colnames)) {
        stop("Neither t nor z (Z) present in coefficients")
    }
    using_overall_t_df <- FALSE
    using_individual_t_df <- FALSE
    if (using_t_not_Z) {
        using_overall_t_df <- "df" %in% names(s)
        using_individual_t_df <- (
            !using_overall_t_df && "df" %in% coeff_colnames
        )
        if (!using_overall_t_df && !using_individual_t_df) {
            stop("Don't know how to extract degrees of freedom for t")
        }
        colname_stat <- "t value"
        colname_p_stat <- "Pr(>|t|)"
    } else {
        colname_stat <- "z value"
        colname_p_stat <- "Pr(>|z|)"
    }

    # Work through the coefficients. Match them to the ANOVA terms.
    coeff_detail <- NULL
    for (i in 1:nrow(coeffs)) {
        term_name <- coeff_rownames[i]
        # The tricky part is assigning them correctly to the ANOVA table terms.
        term_idx <- miscresults$which_anova_term_matches_coeff(
            anova_table$term, term_name
        )
        if (is.na(term_idx)) {
            if (term_name == R_INTERCEPT_LABEL) {
                # For example, the output of
                #   car::Anova(glm(..., family = binomial(link = "logit")))
                # does not have an intercept term.
                anova_term_name <- R_INTERCEPT_LABEL
                term_idx <- 0
            } else {
                cat("--- ERROR. anova_table:\n")
                print(anova_table)
                stop(paste0("Can't match term: ", term_name))
            }
        } else {
            anova_term_name <- anova_table$term[term_idx]
        }
        is_linear <- FALSE
        if (term_name %in% anova_table$term
                || term_name == R_INTERCEPT_LABEL) {
            # Exact match, e.g. linear coefficient.
            # Assign at subterm_idx = 0, i.e. level with the term.
            subterm_idx <- 0
            is_linear <- TRUE
        } else if (is.null(coeff_detail)) {
            # Otherwise (e.g. levels of a factor): start from 1
            subterm_idx <- 1
        } else {
            prev_subterm_idxs <- coeff_detail[
                coeff_detail$term_idx == term_idx,
            ]$subterm_idx
            if (length(prev_subterm_idxs) == 0) {
                subterm_idx <- 1
            } else {
                subterm_idx <- max(prev_subterm_idxs, na.rm = TRUE) + 1
            }
        }
        if (using_overall_t_df) {
            coeff_df_for_t <- s$df[2]
            # ... see summary.lm:
            #       ans$coefficients <- cbind(Estimate = est, `Std. Error` = se,
            #           `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf,
            #           lower.tail = FALSE))
            #       ans$df <- c(p, rdf, NCOL(Qr$qr))
        } else if (using_individual_t_df) {
            coeff_df_for_t <- coeffs[i, "df"]
        } else {
            # e.g. if using Z tests
            coeff_df_for_t <- NA_real_
        }
        coeff_detail <- rbind(
            coeff_detail,
            data.frame(
                coeff_name = term_name,
                is_subterm = TRUE,
                is_linear = is_linear,
                term_idx = term_idx,
                anova_term_name = anova_term_name,
                subterm_idx = subterm_idx,
                coeff = coeffs[i, "Estimate"],
                # ... row, column
                # ... coeffs$`Estimate`[i] fails with
                #     "$ operator is invalid for atomic vectors"
                se = coeffs[i, "Std. Error"],
                coeff_stat = coeffs[i, colname_stat],
                p_coeff_stat = coeffs[i, colname_p_stat],
                coeff_df_for_t = coeff_df_for_t
            )
        )
    }
    coeff_detail$is_reference_level <- FALSE

    # Add reference levels?
    # Inserted at subterm_idx = 0.5, i.e. before the first extracted level.
    use_xlevels <- ("xlevels" %in% names(m))
    use_frame <- ("frame" %in% slotNames(m))
    if (include_reference_levels) {
        for (i in 1:n_anova_terms) {
            term_name <- anova_table$term[i]
            term_idx <- anova_table$term_idx[i]
            if (use_xlevels) {
                if (term_name %in% names(m$xlevels)) {
                    first_level_name <- m$xlevels[[term_name]][1]
                    coeff_detail <- rbind(
                        coeff_detail,
                        data.frame(
                            coeff_name = first_level_name,
                            is_subterm = TRUE,
                            is_linear = FALSE,
                            is_reference_level = TRUE,
                            term_idx = term_idx,
                            anova_term_name = term_name,
                            subterm_idx = 0.5,
                            coeff = NA_real_,
                            se = NA_real_,
                            coeff_stat = NA_real_,
                            p_coeff_stat = NA_real_,
                            coeff_df_for_t = NA_real_
                        )
                    )
                }
            } else if (use_frame) {
                if (term_name %in% names(m@frame)) {
                    factor_levels <- levels(m@frame[[term_name]])
                    if (!is.null(factor_levels)) {
                        first_level_name <- factor_levels[1]
                        coeff_detail <- rbind(
                            coeff_detail,
                            data.frame(
                                coeff_name = first_level_name,
                                is_subterm = TRUE,
                                is_linear = FALSE,
                                is_reference_level = TRUE,
                                term_idx = term_idx,
                                anova_term_name = term_name,
                                subterm_idx = 0.5,
                                coeff = NA_real_,
                                se = NA_real_,
                                coeff_stat = NA_real_,
                                p_coeff_stat = NA_real_,
                                coeff_df_for_t = NA_real_
                            )
                        )
                    }
                }
            } else {
                cat("~~~ Contrasts model:\n")
                print(m)
                stop(paste0(
                    "Don't know how to extract reference levels from this ",
                    "model type"
                ))
            }
        }
    }

    # Add confidence intervals. Also mark intercepts, and set is_term == FALSE
    # for later merging.
    if (using_t_not_Z) {
        conf_int <- miscstat$confidence_interval_from_mu_sem_df_via_t(
            mu = coeff_detail$coeff,
            sem = coeff_detail$se,
            df = coeff_detail$coeff_df_for_t,
            ci = ci
        )
    } else {
        conf_int <- miscstat$confidence_interval_from_mu_sem_via_Z(
            mu = coeff_detail$coeff,
            sem = coeff_detail$se,
            ci = ci
        )
    }
    coeff_detail <- (
        coeff_detail
        %>% mutate(
            ci_lower = conf_int$ci_lower,
            ci_upper = conf_int$ci_upper,
            is_intercept = anova_term_name == R_INTERCEPT_LABEL,
            is_term = FALSE,
            using_t_not_Z = using_t_not_Z
        )
        %>% select(
            # Cosmetic order
            coeff_name,
            term_idx,
            anova_term_name,
            is_term,
            is_subterm,
            subterm_idx,
            is_intercept,
            is_linear,
            is_reference_level,
            coeff,
            se,
            ci_lower,
            ci_upper,
            using_t_not_Z,
            coeff_stat,
            p_coeff_stat,
            coeff_df_for_t
        )
    )
    return(list(
        coeff_summary = s,
        using_t_not_Z = using_t_not_Z,
        coeff_detail = coeff_detail
    ))
}


# -----------------------------------------------------------------------------
# Public interface
# -----------------------------------------------------------------------------

miscresults$mk_model_anova_coeffs <- function(
    model_fn,
    formula,
    data,
    type = "III",
    contrasts_anova_model = NULL,
    contrasts_coeff_model = NULL,
    # Cosmetic:
    include_intercept = TRUE,
    include_reference_levels = TRUE,
    predictor_replacements = NULL,
    coeff_use_plus = TRUE,
    show_ci = TRUE,
    suppress_nonsig_coeffs = TRUE,
    suppress_nonsig_coeff_tests = TRUE,
    keep_intercept_if_suppressing = TRUE,
    squish_up_level_rows = FALSE,
    # Tweaking:
    min_abs_t = miscresults$MINIMUM_ABS_T_SHOWN,
    omit_df_below_min_t = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    min_F = miscresults$MINIMUM_F_SHOWN,
    omit_df_below_min_F = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    ns_text = miscresults$NOT_SIGNIFICANT,
    interaction_txt = paste0(" ", miscresults$MULTIPLY, " "),
    level_combination_text = ", ",
    alpha_show_coeffs = miscresults$DEFAULT_ALPHA,
    reference_label = "Reference",
    level_not_applicable = miscresults$EN_DASH,
    ci = miscresults$DEFAULT_CI,
    show_ss_type = TRUE,
    debug = FALSE,
    # Extras for model_fn:
    ...
) {
    # Format a linear model as a results table (e.g. per style of Cardinal et
    # al. (2023), PMID 37147600, Table 5.) Headings:
    #
    #   Term, Level, F, p_F, coefficient, standard error, Z/t, p_Z/p_t
    #
    # Parameters:
    #
    #   model_fn:
    #       A modelling function: e.g. lm, glm.
    #   formula:
    #       A formula, passed to model_fn.
    #   data:
    #       Data, passed to model_fn.
    #   type:
    #       Type for sums of squares, e.g. "I", "II", "III".
    #       Uses car::Anova for type II/III, or stats::anova for type I [with
    #       limited support, e.g. not for stats::anova(glm(...)) yet].
    #   contrasts_anova_model:
    #       Global contrast options to specify for the ANOVA model. If NULL, do
    #       something sensible, namely:
    #           - when using type III sums of squares, sum-to-zero contrasts
    #             for unordered factors (though using a version that retains
    #             factor names rather than replacing them with numbers), and
    #             polynomial contrasts for ordered predictors;
    #           - default R contrasts otherwise (those being "treatment"
    #             contrasts for unordered factors, and polynomial contrasts for
    #             ordered predictors).
    #
    #       EXPLANATION:
    #
    #       - "Treatment" contrasts are e.g. for a four-level factor,
    #         contr.treatment(4) or contr.treatment(c("A", "B", "C", "D")). The
    #         reference category is 0; others have 1 each. This contrasts each
    #         level with the baseline level, but they are not orthogonal to the
    #         intercept; see ?contr.treatment. (In the contrast matrix: rows
    #         represent levels, and columns are the contrasts.)
    #
    #       - "Sum-to-zero" contrasts are e.g. for a four-level factor,
    #         contr.sum(4) or contr.sum(c("A", "B", "C", "D")). Effect sizes
    #         are not as typically expected. The intercept is at the notional
    #         'zero' level of all factors (not necessarily at any individual
    #         factor level). The reference category is the LAST level of a
    #         factor.
    #
    #       - Try also contr.sum.keepnames(c("A", "B", "C", "D")).
    #
    #       - Polynomial contrasts: try contr.poly(2), showing a linear column
    #         and a quadratic column.
    #
    #       See type_III_sums_of_squares.R as to why.
    #
    #   contrasts_coeff_model:
    #       Global contrast options to specify for the summary model. If NULL,
    #       do something sensible, namely default R contrasts (as above).
    #
    #   include_intercept:
    #       Include a row showing the intercept term of the model.
    #   include_reference_levels:
    #       Include reference levels of factors (though without coefficient
    #       detail, of course).
    #   predictor_replacements:
    #       Vector of replacements to apply to all predictor text, e.g.
    #       c("from1" = "to1", "from2" = "to2", ...), or NULL.
    #   coeff_use_plus:
    #       Show "+" for positive coefficients (and confidence intervals, if
    #       shown).
    #   show_ci:
    #       Show confidence intervals for coefficients?
    #   suppress_nonsig_coeffs:
    #       If the ANOVA F test is not significant, do not show coefficient
    #       rows for individual levels.
    #   suppress_nonsig_coeff_tests:
    #       If the ANOVA F test is not significant, show coefficient rows
    #       (assuming suppress_nonsig_coeff_tests is not TRUE), but do not show
    #       statistical tests of those coefficients.
    #   keep_intercept_if_suppressing:
    #       Retains the intercept coefficient (assuming include_intercept is
    #       TRUE) even if suppress_nonsig_coeffs is TRUE.
    #   squish_up_level_rows:
    #       Ordinarily, we report linear coefficients on the same row as the
    #       ANOVA F test, and levels starting on the row below. If this is set
    #       to TRUE, shift levels up, to save space.
    #
    #   min_abs_t:
    #       See fmt_t_p().
    #   omit_df_below_min_t:
    #       See fmt_t_p().
    #   min_F:
    #       See fmt_F_p().
    #   omit_df_below_min_F:
    #       See fmt_F_p().
    #   ns_text:
    #       Text to indicate "not significant", e.g. "NS".
    #   interaction_txt:
    #       Pretty text to use as interaction symbol.
    #   level_combination_text:
    #       Text to use for indicating combinations of levels.
    #   alpha_show_coeffs:
    #       The alpha value to use for suppress_nonsig_coeffs and
    #       suppress_nonsig_coeff_tests.
    #   reference_label:
    #       If include_reference_levels is TRUE, use this text for the
    #       coefficient column of the reference level.
    #   level_not_applicable:
    #       Text to show in the "Level" column when it's not applicable, i.e.
    #       for continuous predictors.
    #   ci:
    #       If show_ci is true, which confidence intervals to use? Default is
    #       0.95, meaning 95% confidence intervals.
    #   show_ss_type:
    #       Show the type of sum-of-squares calculation used, in the table.
    #   debug:
    #       Be verbose?
    #
    #   ...:
    #       Passed to model_fn.
    #
    # Returns a list with these elements:
    #
    #   anova_model:
    #       Model used for the ANOVA table.
    #       For residual checks, can use e.g.
    #         library(ggpubr)
    #         r <- x$anova_model$residuals  # residuals from relevant model
    #         ggpubr::ggdensity(r)  # visual check of density plot
    #         ggpubr::ggqqplot(r)  # Q-Q plot (you can also split by factors)
    #         shapiro.test(r)  # Shapiro-Wilk test; "significant" = "non-normal"
    #   contrasts_anova_model:
    #       Contrasts used for anova_model.
    #   anova_table:
    #       Original ANOVA table object.
    #   anova_detail:
    #       Internal starting-point version of the ANOVA table. The output of
    #       miscresults$summarize_anova_table().
    #   coeff_model:
    #       Model used to extract coefficients. Use summary() to see the
    #       detail.
    #   contrasts_coeff_model:
    #       Contrasts used for coeff_model.
    #   coeff_summary:
    #       Original R summary() of the coefficient model.
    #   coeff_detail:
    #       Internal starting-point version of the coefficients table. The
    #       output of miscresults$summarize_model_coefficients().
    #   working:
    #       Full-working internal table. (Also used by the
    #       miscresults$summarize_multiple_cph() function.)
    #   table_markdown:
    #       Markdown table, designed to be converted to a flextable.
    #   table_flex:
    #       Version of table_markdown formatted, in basic style, as a flextable
    #       table. You may want to start with table_markdown and process it
    #       yourself, though, for your own table style.

    # -------------------------------------------------------------------------
    # Collate ANOVA and coefficient information
    # -------------------------------------------------------------------------

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Establish what contrasts we'll use
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    using_type_I_ss <- type == "I" || type == 1
    using_type_II_ss <- type == "II" || type == 2
    using_type_III_ss <- type == "III" || type == 3
    if (!using_type_I_ss && !using_type_II_ss && !using_type_III_ss) {
        stop("Bad sum-of-squares type argument")
    }
    formatted_ss_type <- paste0(
        " (via Type ",
        case_when(
            using_type_I_ss ~ "I",
            using_type_II_ss ~ "II",
            using_type_III_ss ~ "III",
            .default = "?"  # impossible as above
        ),
        " sums of squares)"
    )

    r_default_contrasts <- c(
        unordered = "contr.treatment",
        ordered = "contr.poly"
    )
    type_III_contrasts <- c(
        unordered = "contr.sum.keepnames",  # see above
        ordered = "contr.poly"
    )
    if (is.null(contrasts_anova_model)) {
        if (using_type_III_ss) {
            # Should override contrasts for car::Anova using type III SS.
            contrasts_anova_model <- type_III_contrasts
        } else {
            contrasts_anova_model <- r_default_contrasts
        }
    }
    if (is.null(contrasts_coeff_model)) {
        contrasts_coeff_model <- r_default_contrasts
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Run the models, m1 and m2
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    saved_options_contrasts <- getOption("contrasts")  # save
    options(contrasts = contrasts_anova_model)  # set
    m1 <- model_fn(formula, data = data, ...)
    if (identical(contrasts_coeff_model, contrasts_anova_model)) {
        # Same contrasts. Save time:
        m2 <- m1
    } else {
        # Different contrasts.
        options(contrasts = contrasts_coeff_model)  # set
        m2 <- model_fn(formula, data = data, ...)
    }
    options(contrasts = saved_options_contrasts)  # restore

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Extract: ANOVA table object (a) from m1, plus summary (s) and
    # coefficients (coeffs) from m2.
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (using_type_I_ss) {
        a <- stats::anova(m1)
    } else {
        a <- car::Anova(m1, type = type, test.statistic = "F")
        # ... of classes: anova, data.frame
    }

    # -------------------------------------------------------------------------
    # Build our version of the ANOVA table, in intermediate_anova
    # -------------------------------------------------------------------------

    anova_detail <- miscresults$summarize_anova_table(a)
    # If we are using Type III SS, the intercept from the ANOVA model is *not*
    # the same as the intercept from the coefficients model. Do not consider
    # the ANOVA F test for the intercept.
    if (using_type_III_ss) {
        intermediate_anova <- dplyr::filter(anova_detail, !is_intercept)
    } else {
        intermediate_anova <- anova_detail
    }
    n_anova_terms <- nrow(intermediate_anova)

    # -------------------------------------------------------------------------
    # Build our version of the coefficient table, intermediate_coeffs
    # -------------------------------------------------------------------------
    cf <- miscresults$summarize_model_coefficients(
        m2,
        anova_table = intermediate_anova,
        include_reference_levels = include_reference_levels,
        ci = ci
    )
    using_t_not_Z <- cf$using_t_not_Z
    intermediate_coeffs <- cf$coeff_detail

    # -------------------------------------------------------------------------
    # Optionally (but by default), suppress statistical coefficient tests for
    # terms without a significant term in the ANOVA
    # -------------------------------------------------------------------------
    if (suppress_nonsig_coeffs || suppress_nonsig_coeff_tests) {
        if (is.null(alpha_show_coeffs)) {
            stop(paste0(
                "alpha_show_coeffs not specified, but you are using ",
                "suppress_nonsig_coeffs or suppress_nonsig_coeff_tests"
            ))
        }
        for (t_idx in 1:n_anova_terms) {
            if (intermediate_anova[t_idx, ]$pF >= alpha_show_coeffs) {
                if (suppress_nonsig_coeffs) {
                    # Remove this set of coefficients, i.e. keep all others.
                    # We might keep the intercept for this term also.
                    intermediate_coeffs <- (
                        intermediate_coeffs %>%
                        dplyr::filter(
                            term_idx != t_idx
                            | (is_intercept & keep_intercept_if_suppressing)
                        )
                    )
                } else if (suppress_nonsig_coeff_tests) {
                    # Keep the rows, but suppress the significant tests for
                    # these coefficients.
                    rownums <- which(intermediate_coeffs$term_idx == t_idx)
                    intermediate_coeffs[rownums, ]$coeff_stat <- NA
                    intermediate_coeffs[rownums, ]$p_coeff_stat <- NA
                }
            }
        }
    }

    # -------------------------------------------------------------------------
    # Squish things up to save space?
    # -------------------------------------------------------------------------
    if (squish_up_level_rows) {
        intermediate_coeffs <- (
            intermediate_coeffs
            %>% group_by(term_idx)
            %>% arrange(subterm_idx)
            %>% mutate(new_subterm_idx = row_number() - 1)
            %>% ungroup()
            %>% mutate(subterm_idx = new_subterm_idx)
        )
    }

    # -------------------------------------------------------------------------
    # Merge the ANOVA and coefficients tables
    # -------------------------------------------------------------------------
    intermediate <- (
        dplyr::full_join(
            x = intermediate_anova,
            y = intermediate_coeffs,
            by = c("term_idx", "subterm_idx")
        )
        %>% mutate(
            # Fix columns present in both:
            is_intercept = ifelse(
                !is.na(is_intercept.x),
                is_intercept.x,
                is_intercept.y
            ),
            is_term = case_when(
                # It's a term if intermediate_anova says so.
                !is.na(is_term.x) ~ is_term.x,
                .default = is_term.y
            ),
            is_subterm = case_when(
                # It's a subterm if intermediate_coeffs says so.
                is_intercept ~ FALSE,
                !is.na(is_subterm.y) ~ is_subterm.y,
                .default = is_subterm.x
            ),
            # Or not present in both:
            is_reference_level = ifelse(
                is.na(is_reference_level),
                FALSE,
                is_reference_level
            )
        )
        %>% dplyr::select(
            -is_intercept.x, -is_intercept.y,
            -is_term.x, -is_term.y,
            -is_subterm.x, -is_subterm.y
        )
        %>% dplyr::arrange(term_idx, subterm_idx)
    )
    if (!include_intercept) {
        intermediate <- intermediate %>% dplyr::filter(!is_intercept)
    }

    # -------------------------------------------------------------------------
    # Debugging output?
    # -------------------------------------------------------------------------
    if (debug) {
        cat("- ANOVA model:\n")
        print(a)
        cat("\n- Summary of coefficient model:\n")
        print(summary(m2))
        cat("\n- intermediate_anova:\n")
        print(intermediate_anova)
        cat("\n- intermediate_coeffs:\n")
        print(intermediate_coeffs)
        cat("\n- intermediate:\n")
        print(intermediate)
    }

    # -------------------------------------------------------------------------
    # Format the table
    # -------------------------------------------------------------------------
    working <- (
        intermediate
        %>% mutate(
            # Now format.
            # Also fix an oddity: glm() output can produce a coefficient but
            # not an ANOVA term for the intercept, so in that case we move the
            # label to the "term" column.
            formatted_term = case_when(
                is_intercept ~ R_INTERCEPT_LABEL,
                is_term ~ miscresults$fmt_predictor(
                    term,
                    replacements = predictor_replacements,
                    interaction_txt = interaction_txt
                ),
                .default ="",
            ),
            f_txt = case_when(
                is.na(F) ~ "",
                .default = miscresults$fmt_F(
                    F, df, df_resid,
                    min_F = min_F,
                    omit_df_below_min_F = omit_df_below_min_F
                )
            ),
            pf_txt = case_when(
                is.na(pF) ~ "",
                F < min_F ~ ns_text,
                .default = miscresults$mk_p_text_with_label(
                    pF,
                    ns_text = ns_text
                )
            ),
            formatted_level = case_when(
                is_reference_level ~ coeff_name,
                is_intercept ~ level_not_applicable,
                is_subterm ~ miscresults$fmt_level(
                    coeff_name,
                    anova_term_name,
                    replacements = predictor_replacements,
                    interaction_txt = level_combination_text
                ),
                !is.na(coeff) ~ level_not_applicable,
                .default = ""
            ),
            coeff_txt = case_when(
                is_reference_level ~ reference_label,
                is.na(coeff) ~ "",
                show_ci ~ miscresults$fmt_value_ci(
                    x = coeff,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper,
                    use_plus = coeff_use_plus
                ),
                .default = miscresults$fmt_float(
                    coeff,
                    use_plus = coeff_use_plus
                )
            ),
            se_txt = case_when(
                is.na(se) ~ "",
                .default = miscresults$fmt_float(se)
            ),
            coeff_stat_txt = case_when(
                is.na(coeff_stat) ~ "",
                using_t_not_Z ~ miscresults$fmt_t(
                    t = coeff_stat,
                    df = coeff_df_for_t,
                    min_abs_t = min_abs_t,
                    omit_df_below_min_t = omit_df_below_min_t
                ),
                .default = miscresults$fmt_Z(coeff_stat)
            ),
            p_coeff_stat_txt = case_when(
                is.na(p_coeff_stat) ~ "",
                using_t_not_Z & abs(coeff_stat) < min_abs_t ~ ns_text,
                .default = miscresults$mk_p_text_with_label(
                    p_coeff_stat,
                    ns_text = ns_text
                )
            ),
        )
    )
    table_markdown <- (
        working
        %>% select(
            formatted_term,
            f_txt,
            pf_txt,
            formatted_level,
            coeff_txt,
            se_txt,
            coeff_stat_txt,
            p_coeff_stat_txt
        )
    )
    ci_pct <- ci * 100
    colnames(table_markdown) <- c(
        # Prettier versions:
        "Term",
        paste0(
            "*F*",
            ifelse(show_ss_type, formatted_ss_type, "")
        ),
        "*p~F~*",
        "Level",
        ifelse(
            show_ci,
            paste0("Coefficient (", ci_pct, "% CI)"),
            "Coefficient"
        ),
        "Standard error",
        ifelse(using_t_not_Z, "*t*", "*Z*"),
        ifelse(using_t_not_Z, "*p~t~*", "*p~Z~*")
    )

    # -------------------------------------------------------------------------
    # Basic flextable version. (Though users may want to re-process the
    # "table_markdown" component of the output.)
    # -------------------------------------------------------------------------
    table_flex <- miscresults$mk_default_flextable_from_markdown(
        table_markdown
    )

    # -------------------------------------------------------------------------
    # Return the results
    # -------------------------------------------------------------------------
    return(list(
        anova_model = m1,
        contrasts_anova_model = contrasts_anova_model,
        anova_table = a,
        anova_detail = anova_detail,
        coeff_model = m2,
        contrasts_coeff_model = contrasts_coeff_model,
        coeff_summary = cf$coeff_summary,
        coeff_detail = cf$coeff_detail,
        working = working,
        table_markdown = table_markdown,
        table_flex = table_flex
    ))
}


# =============================================================================
# Formatting Cox proportional hazards models
# =============================================================================

# -----------------------------------------------------------------------------
# Internal functions
# -----------------------------------------------------------------------------

miscresults$get_pretty_cph_terms <- function(cph_model, debug = FALSE) {
    # INTERNAL FUNCTION.
    # Takes a Cox proportional hazards model (of type coxph.object), and
    # returns a tibble that will be used by miscresults$mk_cph_table().

    # Core information about the predictors
    all_term_names <- attr(cph_model$terms, "term.labels")
    n_terms <- length(all_term_names)
    factor_level_list <- cph_model$xlevels
    is_factor_logical <- rep(FALSE, length(factor_level_list))
    # ... a list; names are factors; each element is a vector of level names;
    # the first is the reference level. Will be NULL if there are no factors.
    coefficient_names <- names(cph_model$coefficients)
    n_coefficients <- length(coefficient_names)

    # Sanity checks
    if (n_coefficients < 1) {
        stop("don't understand Cox PH model passed to get_pretty_cph_terms")
    }

    # 1. Logical (Boolean) predictors.
    #    For a predictor "x",
    #    - In attr(cph_model$terms, "term.labels") = all_term_names, it appears
    #      as "x".
    #    - In names(cph_model$coefficients) = coefficient_names, it appears as
    #      "xTRUE".
    #    - But the simplest thing is to treat these as factors. They are just
    #      absent from cph_model$xlevels = factor_level_list. But they are
    #      two-level factors, and treating them as such (with reference level
    #      FALSE). So we coerce them into the factor list.
    for (i in 1:n_terms) {
        # There are some potentials for error here if someone uses a variable
        # like xTRUE as a predictor...
        without_true_suffix <- all_term_names[i]
        with_true_suffix <- paste0(without_true_suffix, R_TRUE_TEXT)
        if (with_true_suffix %in% coefficient_names) {
            # We've found one.
            new_logical_predictor <- list(c(R_FALSE_TEXT, R_TRUE_TEXT))
            names(new_logical_predictor) <- without_true_suffix
            factor_level_list <- c(factor_level_list, new_logical_predictor)
            is_factor_logical <- c(is_factor_logical, TRUE)
        }
    }

    # 2. Factors.
    #
    # Empty tibble, for joining.
    # We can use one of these with full_join(), where we can't use NULL.
    factor_names <- names(factor_level_list)  # e.g. "x", "y", "z"; or NULL
    factor_terms_pretty <- tibble(
        term = character(),
        term_plain = character(),
        level = character(),
        level_num = numeric(),
        is_reference_level = logical(),
        is_logical_reflevel = logical(),
        is_factor_reflevel = logical()
    )
    if (!is.null(factor_level_list)) {
        # There are factors present.
        for (t in 1:n_terms) {
            term_plain <- all_term_names[t]  # may be e.g. "x" or "x:y:z"
            components <- str_split_1(term_plain, R_INTERACTION_MARKER)
            n_components <- length(components)
            term_list <- vector("list", n_components)
            term_plain_list <- vector("list", n_components)
            level_list <- vector("list", n_components)
            ref_level_name <- NA_character_
            is_simple_logical <- FALSE
            for (cp in 1:n_components) {
                component <- components[cp]
                factor_idx <- match(component, factor_names)
                term_plain_list[[cp]] <- component
                if (is.na(factor_idx)) {
                    term_list[[cp]] <- component
                    level_list[[cp]] <- NA
                } else {
                    term_list[[cp]] <- paste0(
                        component,
                        factor_level_list[[factor_idx]]
                    )
                    level_list[[cp]] <- factor_level_list[[factor_idx]]
                    if (n_components == 1) {
                        ref_level_name <- factor_level_list[[factor_idx]][1]
                        is_simple_logical <- is_factor_logical[factor_idx]
                    }
                }
            }
            term_grid <- (
                expand.grid(term_list)
                %>% unite("term", everything(), sep = R_INTERACTION_MARKER)
            )
            term_plain_grid <- (
                expand.grid(term_plain_list)
                %>% unite(
                    "term_plain",
                    everything(),
                    sep = R_INTERACTION_MARKER
                )
            )
            level_grid <- (
                expand.grid(level_list)
                %>% unite(
                    "level",
                    everything(),
                    sep = R_INTERACTION_MARKER,
                    na.rm = TRUE
                )
            )
            new_rows <- (
                tibble(
                    term = term_grid$term,
                    term_plain = term_plain_grid$term_plain,
                    level = level_grid$level,
                    is_simple_logical = is_simple_logical,
                    is_reference_level = (
                        !is.na(ref_level_name)
                        & level_grid$level == ref_level_name
                    )
                )
                %>% mutate(
                    is_logical_reflevel = (
                        is_reference_level & is_simple_logical
                    ),
                    is_factor_reflevel = (
                        is_reference_level & !is_logical_reflevel
                    ),
                    level_num = 1:n()
                )
            )
            if (debug) {
                cat("... processing term: ", term_plain, "\n", sep = "")
                cat("    ... new_rows:\n")
                print(new_rows)
            }
            factor_terms_pretty <- rbind(
                factor_terms_pretty,  # accumulated results from previous terms
                new_rows
            )
        }  # loop for terms
    }

    # 3. Add in others, i.e. linear predictors
    coefficient_name_tibble <- (
        tibble(term = coefficient_names)
        %>% mutate(coefficient_num = 1:n())
        # This tibble represents the broad term order of the output.
    )
    joined_terms <- (
        coefficient_name_tibble
        %>% full_join(factor_terms_pretty, by = c("term") )
    )
    terms_pretty <- (
        joined_terms
        %>% mutate(
            level_num = replace_na(level_num, 0),
            term_plain = if_else(is.na(term_plain), term, term_plain),
            is_reference_level = replace_na(is_reference_level, FALSE),
            is_factor_reflevel = replace_na(is_factor_reflevel, FALSE),
            is_logical_reflevel = replace_na(is_logical_reflevel, FALSE),
            involves_interaction = str_detect(term, R_INTERACTION_MARKER)
        )
        %>% group_by(term_plain)
        %>% mutate(term_sort = min(coefficient_num, na.rm = TRUE))
        %>% ungroup()
        %>% arrange(term_sort, level_num)
        %>% mutate(term_num = 1:n())
    )
    if (debug) {
        cat("- all_term_names:\n")
        print(all_term_names)
        cat("- cph_model$xlevels:\n")
        print(cph_model$xlevels)
        cat("- factor_level_list [modified]:\n")
        print(factor_level_list)
        cat("- coefficient_names:\n")
        print(coefficient_names)
        cat("- coefficient_name_tibble:\n")
        print(coefficient_name_tibble)
        cat("- factor_terms_pretty:\n")
        print(factor_terms_pretty)
        cat("- joined_terms:\n")
        print(joined_terms)
        cat("- terms_pretty:\n")
        print(terms_pretty)
    }
    return(
        terms_pretty
        %>% select(
            term_num,
            term,
            term_plain,
            level,
            is_reference_level,
            is_factor_reflevel,
            is_logical_reflevel,
            involves_interaction
        )
    )
}


# -----------------------------------------------------------------------------
# Public interfaces
# -----------------------------------------------------------------------------

miscresults$mk_cph_table <- function(
    cph_model,
    # Cosmetic:
    include_factor_reference_levels = TRUE,
    include_logical_reference_levels = FALSE,
    predictor_replacements = NULL,
    coeff_use_plus = TRUE,
    z_use_plus = TRUE,
    show_ci = TRUE,
    # Tweaking:
    ns_text = miscresults$NOT_SIGNIFICANT,
    interaction_txt = paste0(" ", miscresults$MULTIPLY, " "),
    level_combination_text = ", ",
    reference_label = "Reference",
    level_not_applicable = miscresults$EN_DASH,
    ci = miscresults$DEFAULT_CI,
    debug = FALSE
) {
    # Format a Cox proportional hazards model as a flextable.
    #
    # Parameters:
    #
    #   cph_model:
    #       The Cox proportional hazards model (of type coxph.object).
    #   include_factor_reference_levels:
    #       Include reference levels of factors (though without coefficient
    #       detail, of course).
    #   include_logical_reference_levels:
    #       Include reference levels of logical (Boolean) predictors, i.e. the
    #       "FALSE" level (though without coefficient detail, of course).
    #   predictor_replacements:
    #       Vector of replacements to apply to all predictor text, e.g.
    #       c("from1" = "to1", "from2" = "to2", ...), or NULL.
    #   coeff_use_plus:
    #       Show "+" for positive coefficients.
    #   z_use_plus:
    #       Show "+" for positive Z scores.
    #   show_ci:
    #       Show confidence intervals for coefficients?
    #   ns_text:
    #       Text to indicate "not significant", e.g. "NS".
    #   interaction_txt:
    #       Pretty text to use as interaction symbol.
    #   level_combination_text:
    #       Text to use for indicating combinations of levels.
    #   reference_label:
    #       If include_factor_reference_levels or
    #       include_logical_reference_levels are TRUE, use this text for the
    #       coefficient column of the reference level.
    #   level_not_applicable:
    #       Text to show in the "Level" column when it's not applicable, i.e.
    #       for continuous predictors.
    #   ci:
    #       If show_ci is true, which confidence intervals to use? Default is
    #       0.95, meaning 95% confidence intervals.
    #   debug:
    #       Be verbose?
    #
    # The return value is a list with these elements:
    #
    #   cph_model:
    #       Copied from the input.
    #   working:
    #       Full-working internal table. (Also used by the
    #       miscresults$summarize_multiple_cph() function.)
    #   table_markdown:
    #       Markdown version of the formatted table. Use this one if you want
    #       to create your own custom-formatted flextable, which you probably
    #       do.
    #   table_flex:
    #       A flextable version of the output; it assumes some formatting
    #       parameters, so you probably don't want it for publication, but it
    #       provides a quick look at a "pretty" version of the table.

    s <- summary(cph_model, conf.int = ci)

    # 1. Coefficients
    coeffs <- s$coefficients
    coeffs_intermediate <- (
        coeffs
        %>% as_tibble()
        %>% rename(
            # Just for convenience; remove brackets and other awkwardness.
            # coef -- unchanged
            "coeff" = "coef",
            "exp_coeff" = "exp(coef)",
            "se_coeff" = "se(coef)",
            "z" = "z",
            "p" = "Pr(>|z|)"
        )
        %>% mutate(
            term = coeff_rownames,
            has_coeff = TRUE
        )
    )
    confint <- s$conf.int
    confint_intermediate <- confint %>% as_tibble()
    colnames(confint_intermediate) <- c(
        "exp_coeff", "exp_neg_coeff", "ci_lower", "ci_upper"
    )

    # 2. Confidence intervals.
    # The confidence intervals are on exp(coef). See ?summary.coxph.
    confint_intermediate <- (
        confint_intermediate
        %>% mutate(term = rownames(confint))
        %>% select(-exp_coeff)  # duplication otherwise
    )

    # 3. Prettier term labelling.
    term_pretty <- miscresults$get_pretty_cph_terms(cph_model)

    # Assemble and format.
    working <- (
        term_pretty
        %>% left_join(coeffs_intermediate, by = "term")
        %>% left_join(confint_intermediate, by = "term")
        %>% mutate(
            has_coeff = if_else(is.na(has_coeff), FALSE, has_coeff),
        )
        %>% dplyr::filter(
            has_coeff | !involves_interaction
            # Terms that are interactions AND have no coefficient are not
            # interesting (the reference levels will become obvious from the
            # non-interaction terms, i.e. the plain factors).
        )
    )
    if (!include_factor_reference_levels) {
        working <- working %>% dplyr::filter(!is_factor_reflevel)
    }
    if (!include_logical_reference_levels) {
        working <- working %>% dplyr::filter(!is_logical_reflevel)
    }
    working <- (
        working
        %>% group_by(term_plain)
        %>% mutate(pos_within_term = 1:n())
        %>% ungroup
        %>% mutate(
            txt_term = case_when(
                pos_within_term > 1 ~ "",
                .default = miscresults$fmt_predictor(
                    term_plain,
                    replacements = predictor_replacements,
                    interaction_txt = interaction_txt
                )
            ),
            txt_level = case_when(
                (is.na(level) | level == "") ~ level_not_applicable,
                .default = miscresults$fmt_predictor(
                    level,
                    replacements = predictor_replacements,
                    interaction_txt = level_combination_text
                )
            ),
            txt_coeff = case_when(
                !has_coeff ~ reference_label,
                .default = miscresults$fmt_float(
                    coeff,
                    use_plus = coeff_use_plus
                )
            ),
            txt_exp_coeff = case_when(
                !has_coeff ~ "",
                show_ci ~ miscresults$fmt_value_ci(
                    x = exp_coeff,
                    ci_lower = ci_lower,
                    ci_upper = ci_upper
                ),
                .default = miscresults$fmt_float(exp_coeff)
            ),
            txt_se_coeff = case_when(
                !has_coeff ~ "",
                .default = miscresults$fmt_float(se_coeff)
            ),
            txt_z = case_when(
                !has_coeff ~ "",
                .default = miscresults$fmt_float(z, use_plus = z_use_plus)
            ),
            txt_p = case_when(
                !has_coeff ~ "",
                .default = miscresults$mk_p_text_with_label(
                    p,
                    ns_text = ns_text
                )
            )
        )
    )
    if (debug) {
        print(working)
    }
    table_markdown <- (
        working
        %>% select(
            txt_term,
            txt_level,
            txt_coeff,
            txt_exp_coeff,
            txt_se_coeff,
            txt_z,
            txt_p
        )
        %>% rename(
            # OK to have absent columns here (if CI not being shown).
            "Term" = "txt_term",
            "Level" = "txt_level",
            "Coefficient" = "txt_coeff",
            "*e*^coeff^" = "txt_exp_coeff",
            "SE(coeff)" = "txt_se_coeff",
            "*Z*" = "txt_z",
            "*p*~|*Z*|~" = "txt_p"
        )
    )
    if (all(is.na(table_markdown$Level))) {
        table_markdown <- table_markdown %>% select(-Level)
    }
    table_flex <- miscresults$mk_default_flextable_from_markdown(
        table_markdown
    )
    return(list(
        cph_model = cph_model,
        working = working,
        table_markdown = table_markdown,
        table_flex = table_flex
    ))
}


miscresults$summarize_multiple_cph <- function(
    cph_list,
    up_label = miscresults$UP_ARROW,
    down_label = miscresults$DOWN_ARROW,
    ns_label = miscresults$LEFT_RIGHT_ARROW,
    absent_label = "",
    target_alpha = miscresults$DEFAULT_ALPHA,
    correct_alpha_for = c("none", "models", "tests"),
    alpha_correction_method = c("sidak", "bonferroni")
) {
    # Summarize multiple Cox proportional hazards models, each themselves the
    # output of miscresults$mk_cph_table(). The assumption is that all models
    # have (approximately) the same predictors, but a different dependent
    # variable.
    #
    # Arguments:
    #
    #   cph_list
    #       A list whose elements are each results of
    #       miscresults$mk_cph_table(), and whose names should be used as
    #       column headings. Thus e.g. list("SSRI" = cph_ssri, "Mirtazapine" =
    #       cph_mirtaz). The predictors for each model should correspond
    #       exactly (i.e. they will be matched on exact correspondence).
    #   up_label
    #       Text to label coefficients significantly above zero.
    #   down_label
    #       Text to label coefficients significantly below zero.
    #   ns_label
    #       Text to label coefficients not significantly different from zero.
    #   absent_label
    #       Text to label cells that did not have a test associated with them.
    #   target_alpha
    #       Target familywise alpha; see the next two options.
    #   correct_alpha_for
    #       If "none", do not correct; instead, use target_alpha as the alpha
    #       to consider things "significant" (using the standard method of
    #       declaring a test "significant" if p < alpha). If "models", correct
    #       for the number of models provided (the length of cph_list). If
    #       "tests", which is VERY CONSERVATIVE, correct for the number of
    #       tests performed.
    #   alpha_correction_method
    #       Method for correcting alpha, as above. Use "sidak" for the Sidak
    #       method, which is mathematically correct, or "bonferroni" for the
    #       Bonferroni method, which is incorrect but close.

    n_elements <- length(cph_list)
    if (n_elements < 1) {
        stop("empty input to summarize_multiple_cph")
    }

    correct_alpha_for <- match.arg(correct_alpha_for)
    alpha_correction_method <- match.arg(alpha_correction_method)
    if (correct_alpha_for == "none") {
        n_comparisons <- 1
    } else if (correct_alpha_for == "models") {
        n_comparisons <- length(cph_list)
    } else if (correct_alpha_for == "tests") {
        n_comparisons <- 0
        for (i in 1:n_elements) {
            n_comparisons <- n_comparisons + sum(
                !is.na(cph_list[[i]]$working$p)
            )
        }
    } else {
        stop("bug in summarize_multiple_cph/correct_for")
    }
    if (n_comparisons == 1) {
        corrected_alpha <- target_alpha
    } else if (alpha_correction_method == "sidak") {
        corrected_alpha <- miscstat$sidak_alpha(target_alpha, n_comparisons)
    } else if (alpha_correction_method == "bonferroni") {
        corrected_alpha <- target_alpha / n_comparisons
    } else {
        stop("bug in summarize_multiple_cph/corrected_alpha")
    }

    element_to_tibble <- function(element, name) {
        w <- element$working
        if (is.null(w)) {
            stop("Bad arguments to summarize_multiple_cph")
        }
        w2 <- (
            w
            %>% select(term, txt_term, txt_level, coeff, p)
            %>% mutate(
                label = case_when(
                    p >= corrected_alpha ~ ns_label,
                    coeff > 0 ~ up_label,
                    coeff < 0 ~ down_label,
                    .default = absent_label
                )
            )
            %>% select(term, txt_term, txt_level, label)
            %>% rename(!!name := label)
        )
        return(w2)
    }
    # Create a list with the same names as cph_list, but with the elements
    # now being the result of element_to_tibble(original_element):
    list_of_tables <- imap(cph_list, element_to_tibble)
    # Join them all:
    summary_table <- list_of_tables %>% reduce(
        full_join,
        by = c("term", "txt_term", "txt_level")
    )
    table_markdown <- (
        summary_table
        %>% select(-term)
        %>% rename(
            "Term" = "txt_term",
            "Level" = "txt_level"
        )
    )
    if (all(is.na(table_markdown$Level))) {
        table_markdown <- table_markdown %>% select(-Level)
    }
    table_flex <- miscresults$mk_default_flextable_from_markdown(
        table_markdown
    )
    return(list(
        target_alpha = target_alpha,
        correct_alpha_for = correct_alpha_for,
        alpha_correction_method = alpha_correction_method,
        n_comparisons = n_comparisons,
        corrected_alpha = corrected_alpha,
        summary_table = summary_table,
        table_markdown = table_markdown,
        table_flex = table_flex
    ))
}


# =============================================================================
# Manipulating the display of results tables
# =============================================================================

miscresults$detect_significant_in_result_str <- function(x) {
    # Use to mark significant differences (e.g. in bold).
    # This requires identifying "significance markers", traditionally
    # asterisks. To make this harder, note that asterisks are present in the
    # markdown text! So we're looking for whitespace (\s), one or more
    # asterisks (\*+), end of string ($). Then additional backslashes for R.
    # See example of usage in test_flextable.R.
    stringr::str_detect(x, "\\s\\*+$")
}


miscresults$insert_row <- function(
    .data,
    ...,
    .before = NULL,
    .after = NULL,
    .default = NA,
    .colnum = 1
) {
    # Insert a row into a data frame or similar, before the row specified by
    # .before (range 1 to the number of rows in .data plus 1) or after that
    # specified by .after (range 0 to the number of rows in .data). Default is
    # at the top (equivalent to .before = 1 or .after = 0).
    #
    # This closely resembles tibble::add_row(). However, (a) you can pass a
    # single unnamed variable, and that will then be inserted in column
    # .colnum; (b) you can specify a default that is not NA.

    # Check parameters
    if (!is.data.frame(.data)) {
        stop("miscresults$insert_row: .data must be a data frame")
    }
    if (!is.null(.before) && !is.null(.after)) {
        # Both specified.
        stop("miscresults$insert_row: Can't specify both .before and .after")
    }
    if (is.null(.before) && is.null(.after)) {
        # Neither specified. Insert at top.
        .before <- 1
    }

    # Build new row
    nc <- ncol(.data)
    cn <- colnames(.data)
    if (rlang::dots_n(...) == 0) {
        # No new data. Insert a blank row.
        newrow <- data.frame(matrix(.default, nrow = 1, ncol = nc))
        colnames(newrow) <- cn
    } else {
        dl <- list(...)
        if (length(dl) == 1 && is.null(names(dl))) {
            # Single unnamed new data item.
            if (.colnum < 1 || .colnum > nc) {
                stop("miscresults$insert_row: .colnum out of range of .data")
            }
            newrow <- data.frame(matrix(.default, nrow = 1, ncol = nc))
            newrow[1, .colnum] <- dl[[1]]
            colnames(newrow) <- cn
        } else {
            # Named or multiple new items.
            newrow <- as.data.frame(dl)
            newnames <- colnames(newrow)
            missing_vars <- setdiff(cn, newnames)
            # ... those present in .data but not in the arguments
            if (length(missing_vars) > 0) {
                for (i in 1:length(missing_vars)) {
                    newrow[missing_vars[i]] <- .default
                }
            }
            extra_vars <- setdiff(newnames, cn)
            # ... those present in the arguments but not in .data
            if (length(extra_vars) > 0) {
                stop(paste0(
                    "miscresults$insert_row: columns specified that were not ",
                    "in .data: ",
                    paste(extra_vars, collapse = ", ")
                ))
            }
        }
    }

    # Assemble
    nr <- nrow(.data)
    data_before <- NULL
    data_after <- NULL
    if (!is.null(.before)) {
        if (.before < 1 | .before > nr + 1) {
            stop("miscresults$insert_row: .before out of range of .data")
        }
        if (.before > 1) {
            data_before <- .data[1:(.before - 1), ]
        }
        if (.before <= nr) {
            data_after <- .data[.before:nr, ]
        }
    } else {
        if (.after < 0 || .after > nr) {
            stop("miscresults$insert_row: .after out of range of .data")
        }
        if (.after >= 1) {
            data_before <- .data[1:.after, ]
        }
        if (.after < nr) {
            data_after <- .data[(.after + 1):nr, ]
        }
    }
    return(rbind(data_before, newrow, data_after, make.row.names = FALSE))
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscresults" %in% search()) detach("miscresults")
attach(miscresults)  # subsequent additions not found, so attach at the end
