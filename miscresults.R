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
# - newline: backslash then newline immediately (see miscresults$NEWLINE below)
#
# Less often:
#
# - code: `x`
# - underline: [x]{.underline}


tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    car,  # for car::Anova
    flextable,
    ftExtra,  # for markup within flextable tables
    rcompanion,  # for wilcoxonZ
    tidyverse
)
rm(tmp_require_package_namespace)


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

miscresults <- new.env()


# =============================================================================
# Constants
# =============================================================================

# NOTE RE UNICODE: This can be challenging under Windows. The problem isn't
# usually in handling Unicode, it's in representing it in the file encoding,
# since Windows does not use UTF-8 file encoding by default. It's therefore
# preferable to use "\u<hexcode">, e.g. "\u2013" for an en dash. This is an
# ASCII file representation of a Unicode character, which works fine.

miscresults$CHI_LOWER <- "\u03C7"

miscresults$EN_DASH <- "\u2013"
miscresults$HYPHEN <- "-"  # plain ASCII
miscresults$MINUS <- "\u2212"
miscresults$MULTIPLY <- "\u00D7"
miscresults$MULTIPLICATION_DOT <- "\u22C5"
miscresults$PLUS_MINUS <- "\u00B1"  # case-insensitive

# Traditional order for footnotes:
# - https://en.wikipedia.org/wiki/Note_(typography)
# - asterisk (*), then:
miscresults$DAGGER <- "\u2020"
miscresults$DOUBLE_DAGGER <- "\u2021"
miscresults$SECTION <- "\u00A7"
miscresults$DOUBLE_VERTICAL_LINE <- "\u2016"
miscresults$PILCROW <- "\u00B6"

miscresults$NEWLINE <- "\\\n"  # two characters: backslash, newline

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
    return(ifelse(
        is.nan(x),
        nan_str,
        ifelse(
            is.na(x),
            na_str,
            txt
        )
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
    return(ifelse(
        is.nan(x),
        nan_str,
        ifelse(
            is.na(x),
            na_str,
            txt
        )
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
    ...
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
    return(ifelse(
        is.nan(proportion),
        nan_str,
        ifelse(
            is.na(proportion),
            na_str,
            txt
        )
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
    replacements = NULL,  # e.g. c("from1" = "to1", "from1" = "to2", ...)
    interaction_txt = paste0(" ", miscresults$MULTIPLY, " ")
) {
    # Similar to fmt_predictor(), but for formatting levels; for example,
    # converting "drugLowDose:sexMale" to "Low dose x Male".
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
# Formatting full models
# =============================================================================

miscresults$which_anova_term_matches_coeff <- function(
    anova_terms,
    coeff_term,
    debug = FALSE
) {
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


miscresults$mk_model_anova_coeffs <- function(
    model_fn,
    formula,
    data,
    type = "III",
    contrasts_anova_model = NULL,
    contrasts_coeffs_model = NULL,
    # Cosmetic:
    include_intercept = TRUE,
    include_reference_levels = TRUE,
    predictor_replacements = NULL,
    coeff_use_plus = TRUE,
    show_ci = TRUE,
    suppress_nonsig_coeffs = TRUE,
    suppress_nonsig_coeff_tests = TRUE,
    keep_intercept_if_suppressing = TRUE,
    # Tweaking:
    min_abs_t = miscresults$MINIMUM_ABS_T_SHOWN,
    omit_df_below_min_t = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    min_F = miscresults$MINIMUM_F_SHOWN,
    omit_df_below_min_F = miscresults$DEFAULT_OMIT_DF_BELOW_MIN_STATS,
    ns_text = miscresults$NOT_SIGNIFICANT,
    interaction_txt = paste0(" ", miscresults$MULTIPLY, " "),
    alpha_show_coeffs = miscresults$DEFAULT_ALPHA,
    reference_suffix = " (reference)",
    ci = miscresults$DEFAULT_CI,
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
    #       Type for sums of squares, via car::Anova, e.g. "II", "III"
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
    #   contrasts_coeffs_model:
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
    #   alpha_show_coeffs:
    #       The alpha value to use for suppress_nonsig_coeffs and
    #       suppress_nonsig_coeff_tests.
    #   reference_suffix:
    #       If include_reference_levels, append this suffix to the name of the
    #       reference level.
    #   ci:
    #       If show_ci is true, which confidence intervals to use? Default is
    #       0.95, meaning 95% confidence intervals.
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
    #   contrasts_anova_model:
    #       Contrasts used for anova_model.
    #   coeffs_model:
    #       Model used to extract coefficients. Use summary() to see the
    #       detail.
    #   contrasts_coeffs_model:
    #       Contrasts used for coeffs_model.
    #   table_markdown:
    #       Markdown table, designed to be converted to a flextable.
    #   table_flex:
    #       Version of table_markdown formatted, in basic style, as a flextable
    #       table. You may want to start with table_markdown and process it
    #       yourself, though, for your own table style.

    # -------------------------------------------------------------------------
    # Collate ANOVA and coefficient information
    # -------------------------------------------------------------------------

    r_default_contrasts <- c(
        unordered = "contr.treatment",
        ordered = "contr.poly"
    )
    type_III_contrasts <- c(
        unordered = "contr.sum.keepnames",  # see above
        ordered = "contr.poly"
    )
    if (is.null(contrasts_anova_model)) {
        if (type == "III" || type == 3) {
            # Should override contrasts for car::Anova using type III SS.
            contrasts_anova_model <- type_III_contrasts
        } else {
            contrasts_anova_model <- r_default_contrasts
        }
    }
    if (is.null(contrasts_coeffs_model)) {
        contrasts_coeffs_model <- r_default_contrasts
    }

    saved_options_contrasts <- getOption("contrasts")  # save
    options(contrasts = contrasts_anova_model)  # set
    m1 <- model_fn(formula, data = data, ...)
    options(contrasts = contrasts_coeffs_model)  # set
    m2 <- model_fn(formula, data = data, ...)
    options(contrasts = saved_options_contrasts)  # restore

    a <- car::Anova(m1, type = type, test.statistic = "F")
    # ... of classes: anova, data.frame
    s <- summary(m2)
    # ... of class e.g. summary.lm, and of mode list
    coeffs <- s$coefficients
    # ... of classes: matrix, array
    # For some summaries of models from glm(), coefficient headings may be
    #       Estimate, Std. Error, z value, Pr(>|z|)
    # For others, e.g. from lm(), may be
    #       Estimate, Std. Error, t value, Pr(>|t|)
    using_t_not_Z <- "t value" %in% colnames(coeffs)
    if (!using_t_not_Z && !("z value" %in% colnames(coeffs))) {
        stop("Neither t nor z (Z) present in coefficients")
    }
    coeff_rdf_for_t <- s$df[2]
    # ... see summary.lm:
    #       ans$coefficients <- cbind(Estimate = est, `Std. Error` = se,
    #           `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf,
    #           lower.tail = FALSE))
    #       ans$df <- c(p, rdf, NCOL(Qr$qr))

    # -------------------------------------------------------------------------
    # Build our version of the ANOVA table
    # -------------------------------------------------------------------------
    intermediate_anova <- NULL
    nrow_anova <- nrow(a)
    stopifnot(rownames(a)[nrow_anova] == R_RESIDUALS_LABEL)
    df_resid <- a$Df[nrow_anova]
    for (i in 1:nrow_anova) {
        term_name <- rownames(a)[i]
        if (term_name == R_RESIDUALS_LABEL) {
            next  # but will be the end; residuals come last
        }
        intermediate_anova <- rbind(
            intermediate_anova,
            data.frame(
                term = term_name,
                is_subterm = FALSE,
                F = a$`F value`[i],
                df = a$Df[i],
                pF = a$`Pr(>F)`[i],
                is_intercept = term_name == R_INTERCEPT_LABEL
            )
        )
    }

    # The intercept from the ANOVA model is *not* the same as the intercept
    # from the coefficients model. Do not consider the ANOVA F test for the
    # intercept.
    intermediate_anova <- filter(intermediate_anova, !is_intercept)

    n_anova_terms <- nrow(intermediate_anova)
    intermediate_anova$term_idx <- 1:n_anova_terms
    intermediate_anova$subterm_idx <- 0

    # -------------------------------------------------------------------------
    # Build our version of the coefficient table
    # -------------------------------------------------------------------------
    ci_pct <- ci * 100
    intermediate_coeffs <- NULL
    for (i in 1:nrow(coeffs)) {
        term_name <- rownames(coeffs)[i]
        # The tricky part is assigning them correctly to the ANOVA table terms.
        term_idx <- miscresults$which_anova_term_matches_coeff(
            intermediate_anova$term, term_name
        )
        if (is.na(term_idx)) {
            if (term_name == R_INTERCEPT_LABEL) {
                # For example, the output of
                #   car::Anova(glm(..., family = binomial(link = "logit")))
                # does not have an intercept term.
                anova_term_name <- R_INTERCEPT_LABEL
                term_idx <- 0
            } else {
                cat("--- ERROR. intermediate_anova:\n")
                print(intermediate_anova)
                stop(paste0("Can't match term: ", term_name))
            }
        } else {
            anova_term_name <- intermediate_anova$term[term_idx]
        }
        if (term_name %in% intermediate_anova$term
                || term_name == R_INTERCEPT_LABEL) {
            # Exact match
            subterm_idx <- 0
        } else if (is.null(intermediate_coeffs)) {
            subterm_idx <- 1
        } else {
            prev_subterm_idxs <- intermediate_coeffs[
                intermediate_coeffs$term_idx == term_idx,
            ]$subterm_idx
            if (length(prev_subterm_idxs) == 0) {
                subterm_idx <- 1
            } else {
                subterm_idx <- max(prev_subterm_idxs, na.rm = TRUE) + 1
            }
        }
        intermediate_coeffs <- rbind(
            intermediate_coeffs,
            data.frame(
                level = term_name,
                is_subterm = TRUE,
                term_idx = term_idx,
                anova_term_name = anova_term_name,
                subterm_idx = subterm_idx,
                coeff = coeffs[i, 1],
                # ... row, column
                # ... coeffs$`Estimate`[i] fails with
                #     "$ operator is invalid for atomic vectors"
                se = coeffs[i, 2],
                coeff_stat = coeffs[i, 3],
                p_coeff_stat = coeffs[i, 4]
            )
        )
    }

    # Add reference levels?
    intermediate_coeffs$is_reference_level <- FALSE
    if (include_reference_levels) {
        for (i in 1:n_anova_terms) {
            term_name <- intermediate_anova$term[i]
            term_idx <- intermediate_anova$term_idx[i]
            if (term_name %in% names(m2$xlevels)) {
                first_level_name <- m2$xlevels[[term_name]][1]
                intermediate_coeffs <- rbind(
                    intermediate_coeffs,
                    data.frame(
                        level = first_level_name,
                        is_subterm = TRUE,
                        is_reference_level = TRUE,
                        term_idx = term_idx,
                        anova_term_name = term_name,
                        subterm_idx = 0.5,
                        coeff = NA_real_,
                        se = NA_real_,
                        coeff_stat = NA_real_,
                        p_coeff_stat = NA_real_
                    )
                )
            }
        }
    }

    # Add confidendence intervals. Also mark intercepts.
    if (using_t_not_Z) {
        conf_int <- miscstat$confidence_interval_from_mu_sem_df_via_t(
            mu = intermediate_coeffs$coeff,
            sem = intermediate_coeffs$se,
            df = coeff_rdf_for_t,
            ci = ci
        )
    } else {
        conf_int <- miscstat$confidence_interval_from_mu_sem_via_Z(
            mu = intermediate_coeffs$coeff,
            sem = intermediate_coeffs$se,
            ci = ci
        )
    }
    intermediate_coeffs <- (
        intermediate_coeffs
        %>% mutate(
            ci_lower = conf_int$ci_lower,
            ci_upper = conf_int$ci_upper,
            is_intercept = anova_term_name == R_INTERCEPT_LABEL
        )
    )

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
                        filter(
                            term_idx != t_idx
                            | (is_intercept & keep_intercept_if_suppressing)
                        )
                    )
                } else if (suppress_nonsig_coeff_tests) {
                    rownums <- which(intermediate_coeffs$term_idx == t_idx)
                    intermediate_coeffs[rownums, ]$coeff_stat <- NA
                    intermediate_coeffs[rownums, ]$p_coeff_stat <- NA
                }
            }
        }
    }

    # -------------------------------------------------------------------------
    # Merge the ANOVA and coefficients
    # -------------------------------------------------------------------------
    intermediate <- (
        dplyr::full_join(
            x = intermediate_anova,
            y = intermediate_coeffs,
            by = c("term_idx", "subterm_idx")
        )
        %>% mutate(
            is_subterm = ifelse(
                is_intercept,
                FALSE,
                ifelse(
                    !is.na(is_subterm.x),
                    is_subterm.x,
                    is_subterm.y
                )
            )
        )
        %>% dplyr::select(-is_subterm.x, -is_subterm.y)
        %>% dplyr::arrange(term_idx, subterm_idx)
    )
    if (!include_intercept) {
        intermediate <- intermediate %>% filter(!is_intercept)
    }

    # -------------------------------------------------------------------------
    # Debugging output?
    # -------------------------------------------------------------------------
    if (debug) {
        cat("* miscresults$fmt_lm:\n")
        cat("- ANOVA:\n")
        print(a)
        cat("\n- Summary:\n")
        print(s)
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
    resultstable <- (
        intermediate
        %>% mutate(
            # Now format.
            vector_using_t_not_Z = using_t_not_Z,  # see below
            vector_show_ci = ci,
            # Also fix an oddity: glm() output can produce a coefficient but
            # not an ANOVA term for the intercept, so in that case we move the
            # label to the "term" column.
            formatted_term = ifelse(
                is_intercept,
                R_INTERCEPT_LABEL,
                ifelse(
                    is_subterm,
                    "",
                    miscresults$fmt_predictor(
                        term,
                        replacements = predictor_replacements,
                        interaction_txt = interaction_txt
                    )
                )
            ),
            formatted_level = ifelse(
                is_intercept,
                "",
                ifelse(
                    is_subterm,
                    paste0(
                        miscresults$fmt_level(
                            level,
                            anova_term_name,
                            replacements = predictor_replacements,
                            interaction_txt = interaction_txt
                        ),
                        ifelse(is_reference_level, reference_suffix, "")
                    ),
                    ""
                )
            ),
            f_txt = ifelse(
                is.na(F),
                "",
                miscresults$fmt_F(
                    F, df, df_resid,
                    min_F = min_F,
                    omit_df_below_min_F = omit_df_below_min_F
                )
            ),
            pf_txt = ifelse(
                is.na(pF),
                "",
                ifelse(
                    F < min_F,
                    ns_text,
                    miscresults$mk_p_text_with_label(pF, ns_text = ns_text)
                )
            ),
            coeff_txt = ifelse(
                is.na(coeff),
                "",
                ifelse(
                    vector_show_ci,
                    miscresults$fmt_value_ci(
                        x = coeff,
                        ci_lower = ci_lower,
                        ci_upper = ci_upper,
                        use_plus = coeff_use_plus
                    ),
                    miscresults$fmt_float(coeff, use_plus = coeff_use_plus)
                )
            ),
            se_txt = ifelse(
                is.na(se),
                "",
                miscresults$fmt_float(se)
            ),
            coeff_stat_txt = ifelse(
                is.na(coeff_stat),
                "",
                ifelse(
                    vector_using_t_not_Z,  # must be PARALLEL as above
                    miscresults$fmt_t(
                        t = coeff_stat,
                        df = coeff_rdf_for_t,
                        min_abs_t = min_abs_t,
                        omit_df_below_min_t = omit_df_below_min_t
                    ),
                    miscresults$fmt_Z(coeff_stat)
                )
            ),
            p_coeff_stat_txt = ifelse(
                is.na(p_coeff_stat),
                "",
                ifelse(
                    vector_using_t_not_Z & abs(coeff_stat) < min_abs_t,
                    ns_text,
                    miscresults$mk_p_text_with_label(
                        p_coeff_stat,
                        ns_text = ns_text
                    )
                )
            ),
        )
        %>% select(
            formatted_term,
            formatted_level,
            f_txt,
            pf_txt,
            coeff_txt,
            se_txt,
            coeff_stat_txt,
            p_coeff_stat_txt
        )
    )
    colnames(resultstable) <- c(
        # Prettier versions:
        "Term",
        "Level",
        "*F*",
        "*p~F~*",
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
    ft <- (
        resultstable
        %>% flextable()
        %>% ftExtra::colformat_md(part = "all")
        # ... need part = "all" to affect headey and body
        %>% flextable::valign(valign = "top")  # align all cells top
        %>% autofit()  # size columns
    )

    # -------------------------------------------------------------------------
    # Return the results
    # -------------------------------------------------------------------------
    return(list(
        anova_model = m1,
        contrasts_anova_model = contrasts_anova_model,
        coeffs_model = m2,
        contrasts_coeffs_model = contrasts_coeffs_model,
        table_markdown = resultstable,
        table_flex = ft
    ))
}


# =============================================================================
# Manipulating display
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


miscresults$insert_caption_row <- function(
    markdown_table,
    caption,
    rownum_to_precede = 1,
    colnum = 1
) {
    # Insert a caption row into a markdown table, before the row specified by
    # rownum_to_precede, with text in the column specified by colnum.

    nr <- nrow(markdown_table)
    stopifnot(rownum_to_precede >= 1 && rownum_to_precede <= nr + 1)
    # Build new row
    nc <- ncol(markdown_table)
    m <- data.frame(matrix("", nrow = 1, ncol = nc))
    colnames(m) <- colnames(markdown_table)
    m[1, colnum] <- caption
    # Reassemble
    result <- NULL
    if (rownum_to_precede > 1) {
        result <- rbind(result, markdown_table[1:rownum_to_precede, ])
    }
    result <- rbind(result, m)
    if (rownum_to_precede <= nr) {
        result <- rbind(result, markdown_table[rownum_to_precede:nr, ])
    }
    return(result)
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscresults" %in% search()) detach("miscresults")
attach(miscresults)  # subsequent additions not found, so attach at the end
