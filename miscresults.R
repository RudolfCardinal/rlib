# miscresults.R
#
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

tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    flextable,
    ftExtra,  # for markup within flextable tables
    rcompanion,  # for wilcoxonZ
    stringr
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
# ... ftExtra uses Pandoc markdown;
# - https://www.uv.es/wikibase/doc/cas/pandoc_manual_2.7.3.wiki?53
# - https://ftextra.atusy.net/articles/format_columns

miscresults$DEFAULT_DP_FOR_DF <- 1  # decimal places for non-integer degrees of freedom
miscresults$MINIMUM_P_SHOWN <- 2.2e-16
    # .Machine$double.eps is 2.220446e-16; however, readers are used to seeing
    # "2.2e-16" or equivalent representations in output from R, not "2.22e-16".
miscresults$NOT_SIGNIFICANT <- "NS"


# =============================================================================
# Helper functions
# =============================================================================

miscresults$is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    # per example in ?base::integer
    abs(x - round(x)) < tol
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
        if (!is.na(scimatch[1])) {
            # Using scientific notation. Make it pretty!
            radix <- scimatch[2]
            sign <- scimatch[3]
            sign <- stringr::str_replace(sign, "[+]", "")
            # ... remove + from exponent
            exponent <- stringr::str_replace(scimatch[4], "^0+" ,"")
            # ... remove leading zeros from exponent
            # Use ^...^ for superscript with ftExtra.
            txt <- paste0(
                radix, " ", miscresults$MULTIPLY, " 10^", sign, exponent, "^"
            )
        }
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
    minimum_shown = miscresults$MINIMUM_P_SHOWN
) {
    # From a p value, return a string such as "*p* = 0.03" or "*p* < 2.2e-16".
    # Uses fmt_float().
    return(ifelse(
        p < minimum_shown,
        paste0(
            "*p* < ",
            miscresults$fmt_float(minimum_shown, sf = sf)
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
    minimum_shown = miscresults$MINIMUM_P_SHOWN,
    ns_text = miscresults$NOT_SIGNIFICANT
) {
    # From a p value, return an asterisk-labelled string such as "*p* = 0.03 *"
    # or "*p* = 0.5, NS". Optionally, apply a Sidak correction to the labelling
    # (without altering the p value itself).
    return(paste0(
        miscresults$mk_p_text(
            p,
            sf = sf,
            minimum_shown = minimum_shown
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
    big.mark = get_flextable_defaults()$big.mark,
    decimal.mark = get_flextable_defaults()$decimal.mark
) {
    # Format degrees of freedom (df) appropriately -- as an exact integer, or
    # to 1 dp if it is not integer (since knowing the fact of not being an
    # integer is often quite important!).
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


miscresults$fmt_mean_sd <- function(
    mu,
    sigma,
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE,
    mean_prefix = "",
    # sd_prefix = paste0(" (", miscresults$PLUS_MINUS, " "),
    sd_prefix = " (SD ",
    sd_suffix = ")",
    na_str = get_flextable_defaults()$na_str
) {
    # Given a mean mu and a standard deviation sigma, show this as "μ (± σ)",
    # or similar.
    # See also flextable::fmt_avg_dev(avg, dev), which is less flexible.
    m_text <- miscresults$fmt_float(
        mu, sf = sf, allow_sci_notation = allow_sci_notation
    )
    s_text <- miscresults$fmt_float(
        sigma, sf = sf, allow_sci_notation = allow_sci_notation
    )
    results <- paste0(mean_prefix, m_text, sd_prefix, s_text, sd_suffix)
    return(ifelse(is.na(mu), na_str, results))
}


miscresults$mk_mean_sd <- function(
    x,
    na.rm = TRUE,
    ...
) {
    # Calculate a mean and standard deviation (SD) from the vector provided, and
    # show this as "μ (± σ)", or similar. Additional parameters are passed to
    # fmt_mean_sd().
    return(miscresults$fmt_mean_sd(
        mu = mean(x, na.rm = na.rm),
        sigma = sd(x, na.rm = na.rm),
        ...
    ))
}


miscresults$fmt_mean_ci <- function(
    mu,
    ci_lower,
    ci_upper,
    mean_prefix = "",
    ci_prefix = " (CI ",
    ci_sep = " to ",  # miscresults$EN_DASH also possible
    ci_suffix = ")",
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE,
    na_str = get_flextable_defaults()$na_str
) {
    # Given a mean mu and confidence interval limits ci_lower, ci_upper, show
    # this as e.g. "μ (a–b)".
    results <- paste0(
        mean_prefix,
        miscresults$fmt_float(
            mu,
            sf = sf, allow_sci_notation = allow_sci_notation
        ),
        ci_prefix,
        miscresults$fmt_float(
            ci_lower,
            sf = sf, allow_sci_notation = allow_sci_notation
        ),
        ci_sep,
        miscresults$fmt_float(
            ci_upper,
            sf = sf, allow_sci_notation = allow_sci_notation
        ),
        ci_suffix
    )
    return(ifelse(is.na(mu), na_str, results))
}


miscresults$mk_mean_ci <- function(
    x,
    ci = 0.95,
    na.rm = TRUE,
    ...
) {
    # From a vector, show a mean and confidence interval (e.g. 95% CI) as e.g.
    # "μ (a–b)". Additional parameters are passed to fmt_mean_ci().
    mu <- mean(x, na.rm = na.rm)
    ci_pair <- miscstat$confidence_interval_t(x, ci = ci, na.rm = na.rm)
    ci_lower <- ci_pair["ci_lower"]
    ci_upper <- ci_pair["ci_upper"]
    return(miscresults$fmt_mean_ci(
        mu = mu,
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


# =============================================================================
# Formatted statistical tests
# =============================================================================

miscresults$mk_chisq_contingency <- function(
    x_counts,
    y_counts,
    minimum_chisq_shown = 1,
    # ... critical value is qchisq(0.95, df) at α=0.05 and df=1, increasing
    #     for higher df. We might want to report things that didn't
    #     make it, but 1 seems like a reasonable "definitely do not care"
    #     threshold.
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = 0.05,
    debug = FALSE,
    ...
) {
    # Reports chi-squared to 1 dp.
    # Both x_counts and y_counts should be vectors of integers. (They are not
    # named x and y because of the differing syntax of chisq.test for x-and-y
    # rather than the contingency table/matrix form.)
    # Any additional parameters are passed to chisq.test().
    d <- matrix(c(x_counts, y_counts), nrow = 2)
    result <- chisq.test(d, ...)
    if (debug) {
        print(result)
    }
    chisq <- result$statistic
    chisq_txt <- miscresults$fmt_float(chisq)
    df_txt <- miscresults$mk_df_text(result$parameter)
    chisq_symbol_df_txt <- sprintf(
        paste0("*", miscresults$CHI_LOWER, "*^2^~%s~"),
        df_txt
    )
    p <- result$p.value
    # NB chisq can only be positive.
    if (chisq < minimum_chisq_shown) {
        if (p <= check_alpha) {
            stop(sprintf(
                "chisq (%f) < %f but p = %f so disallowing shortcut",
                chisq, minimum_chisq_shown, p
            ))
        }
        min_chisq_txt <- miscresults$fmt_float(minimum_chisq_shown)
        return(sprintf(
            "%s < %s, %s", chisq_symbol_df_txt, min_chisq_txt, ns_text
        ))
    }
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    return(sprintf("%s = %s, %s", chisq_symbol_df_txt, chisq_txt, p_txt))
}


miscresults$mk_t_test <- function(
    x,
    y = NULL,
    minimum_abs_t_shown = 1,
    # ... critical value for a two-tailed test at α = 0.05 is ±qt(0.975, df),
    #     decreasing for higher df, and approaching qnorm(0.975) as df → ∞.
    #     So that's about 1.96. We might want to report things that didn't
    #     make it, but 1 seems like a reasonable "definitely do not care"
    #     threshold.
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = 0.05,
    debug = FALSE,
    ...
) {
    # Reports a t test. Any additional parameters are passed to t.test().
    # The direction of Z is "x - y" (i.e. positive if x > y, negative if x < y).
    result <- t.test(x = x, y = y, ...)
    if (debug) {
        print(result)
    }
    t <- result$statistic
    t_txt <- miscresults$fmt_float(t, use_plus = TRUE)
    df_txt <- miscresults$mk_df_text(result$parameter)
    t_symbol_df_txt <- sprintf("*t*~%s~", df_txt)
    p <- result$p.value
    if (abs(t) < minimum_abs_t_shown) {
        if (p <= check_alpha) {
            stop(sprintf(
                "t = %f so |t| < %f but p = %f so disallowing shortcut",
                t, minimum_abs_t_shown, p
            ))
        }
        min_t_txt <- miscresults$fmt_float(minimum_abs_t_shown)
        # Note the |t| symbol below.
        return(sprintf("|%s| < %s, %s", t_symbol_df_txt, min_t_txt, ns_text))
    }
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    return(sprintf("%s = %s, %s", t_symbol_df_txt, t_txt, p_txt))
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
    # Follows argument convension for wilcox.test(); any additional parameters
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
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    z_txt <- miscresults$fmt_float(z, use_plus = TRUE)
    return(sprintf("*W* = %s, *Z* = %s, %s", w_txt, z_txt, p_txt))
}


miscresults$mk_oneway_anova <- function(
    depvar,
    factorvar,
    ns_text = miscresults$NOT_SIGNIFICANT,
    debug = FALSE,
    ...
) {
    # Reports a one-way ANOVA predicting depvar by factorvar.
    # - Note that for one-way ANOVA, the sum of squares "type" is not
    #   applicable, since there is only one predictor.
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
    # No commas in degrees of freedom (big.mark = ""), since they are separated
    # by commas anyway.
    df_num_txt <- miscresults$mk_df_text(df_num, big.mark = "")
    df_denom_txt <- miscresults$mk_df_text(df_denom, big.mark = "")
    F_symbol_df_txt <- sprintf("*F*~%s,%s~", df_num_txt, df_denom_txt)
    F_txt <- miscresults$fmt_float(F)
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    return(sprintf("%s = %s, %s", F_symbol_df_txt, F_txt, p_txt))
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

# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscresults" %in% search()) detach("miscresults")
attach(miscresults)  # subsequent additions not found, so attach at the end
