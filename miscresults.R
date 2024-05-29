# miscresults.R

tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    flextable,
    ftExtra,  # for markup within flextable tables
    stringr
)
rm(tmp_require_package_namespace)


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscresults <- new.env()


#==============================================================================
# Constants
#==============================================================================

miscresults$HYPHEN <- "-"
miscresults$MINUS <- "−"
miscresults$EN_DASH <- "–"
miscresults$PLUS_MINUS <- "±"
miscresults$MULTIPLICATION_DOT <- "⋅"

miscresults$DEFAULT_DP_FOR_DF <- 1  # decimal places for non-integer degrees of freedom
miscresults$MINIMUM_P_SHOWN <- 2.2e-16
    # .Machine$double.eps is 2.220446e-16; however, readers are used to seeing
    # "2.2e-16" or equivalent representations in output from R, not "2.22e-16".
miscresults$NOT_SIGNIFICANT <- "NS"


#==============================================================================
# Formatting results: basic conversion to "pretty" text formats
#==============================================================================

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


miscresults$fmt_float <- function(
    x,
    allow_sci_notation = TRUE,
    sf = get_flextable_defaults()$digits,
    big.mark = get_flextable_defaults()$big.mark,
    decimal.mark = get_flextable_defaults()$decimal.mark,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str
) {
    # Format a floating-point (real) number, according to a number of
    # significant figures, allowing scientific notation or not. Return values
    # might look like "0.1", "2.2 × 10^−16^" (the latter using ftExtra markup).
    # An extension for flextable::fmt_dbl, using its notation.

    if (allow_sci_notation) {
        txt <- formatC(
            signif(x, digits = sf),
            digits = sf,
            format = "g",
            big.mark = big.mark,
            decimal.mark = decimal.mark
        )
        # May or may not be in scientific notation.
        # If scientific notation:
        scimatch <- stringr::str_match(txt, "(.+)e(-?)(\\d+)")
        if (!is.na(scimatch[1])) {
            # Using scientific notation. Make it pretty!
            radix <- scimatch[2]
            sign <- scimatch[3]
            exponent <- stringr::str_replace(scimatch[4], "^0+" ,"")
            # ... remove leading zeros from exponent
            # Use ^...^ for superscript with ftExtra.
            txt <- paste0(radix, " × 10^", sign, exponent, "^")
        }
    } else {
        # https://stackoverflow.com/questions/3245862
        txt <- formatC(
            signif(x, digits = sf),
            digits = sf,
            format = "fg",
            flag = "#",
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


miscresults$mk_df_text <- function(df, dp = miscresults$DEFAULT_DP_FOR_DF) {
    # Format degrees of freedom (df) appropriately -- as an exact integer, or
    # to 1 dp if it is not integer (since knowing the fact of not being an
    # integer is often quite important!).
    return(ifelse(
        as.integer(df) == df,
        flextable::fmt_int(df),  # integer version
        formatC(df, format = "f", digits = dp)  # floating-point version
    ))
}


miscresults$mk_n_percent <- function(
    n,
    total,
    ...
) {
    # Format a number as "n (x%)", where x is the percentage form of n / total.
    # Additional parameters are passed to fmt_n_percent().
    proportion <- n / total
    return(flextable::fmt_n_percent(n, proportion, ...))
}


miscresults$fmt_mean_sd <- function(
    mu,
    sigma,
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE,
    with_brackets = TRUE,
    with_plus_minus = TRUE
) {
    # Given a mean mu and a standard deviation sigma, show this as "μ (± σ)",
    # or "μ ± σ" if with_brackets is FALSE. If with_plus_minus is FALSE, omit
    # "±" (but brackets required).
    # See also flextable::fmt_avg_dev(avg, dev), which is less flexible.
    if (!with_brackets && !with_plus_minus) {
        stop(
            "Parameters with_brackets and with_plus_minus cannot both be FALSE"
        )
    }
    m_text <- miscresults$fmt_float(
        mu, sf = sf, allow_sci_notation = allow_sci_notation
    )
    s_text <- miscresults$fmt_float(
        sigma, sf = sf, allow_sci_notation = allow_sci_notation
    )
    if (with_plus_minus) {
        s_group <- paste(miscresults$PLUS_MINUS, s_text)
    } else {
        s_group <- s_text
    }
    if (with_brackets) {
        return(sprintf("%s (%s)", m_text, s_group))
    } else {
        return(sprintf("%s %s", m_text, s_group))
    }
}


miscresults$mk_mean_sd <- function(
    x,
    na.rm = TRUE,
    ...
) {
    # Calculate a mean and standard deviation (SD) from the vector provided, and
    # show this as "μ (± σ)", or similar. Additional parameters are passed to
    # fmt_mean_sd().
    return(fmt_mean_sd(
        mu = mean(x, na.rm = na.rm),
        sigma = sd(x, na.rm = na.rm),
        ...
    ))
}


miscresults$fmt_mean_ci <- function(
    mu,
    ci_lower,
    ci_upper,
    range_text = miscresults$EN_DASH,  # " to " or ", " are also sensible
    ci_prefix = "(",
    ci_suffix = ")",
    sf = get_flextable_defaults()$digits,
    allow_sci_notation = TRUE
) {
    # Given a mean mu and confidence interval limits ci_lower, ci_upper, show
    # this as e.g. "μ (a–b)".
    return(paste0(
        miscresults$fmt_float(
            mu,
            sf = sf, allow_sci_notation = allow_sci_notation
        ),
        " ",
        ci_prefix,
        miscresults$fmt_float(
            ci_lower,
            sf = sf, allow_sci_notation = allow_sci_notation
        ),
        range_text,
        miscresults$fmt_float(
            ci_upper,
            sf = sf, allow_sci_notation = allow_sci_notation
        ),
        ci_suffix
    ))
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
    return(fmt_mean_ci(
        mu = mu,
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        ...
    ))
}


#==============================================================================
# Formatted statistical tests
#==============================================================================

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
    debug = FALSE
) {
    # Reports chi-squared to 1 dp.
    # Both x_counts and y_counts should be vectors of integers. (They are not
    # named x and y because of the differing syntax of chisq.test for x-and-y
    # rather than the contingency table/matrix form.)
    d <- matrix(c(x_counts, y_counts), nrow = 2)
    result <- chisq.test(d)
    if (debug) {
        print(result)
    }
    chisq <- result$statistic
    chisq_txt <- miscresults$fmt_float(chisq)
    df_txt <- miscresults$mk_df_text(result$parameter)
    chisq_symbol_df_txt <- sprintf("*Χ*^2^~%s~", df_txt)
    p <- result$p.value
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
    paired = FALSE,
    var.equal = FALSE,
    conf.level = 0.95,
    minimum_abs_t_shown = 1,
    # ... critical value for a two-tailed test at α = 0.05 is ±qt(0.975, df),
    #     decreasing for higher df, and approaching qnorm(0.975) as df → ∞.
    #     So that's about 1.96. We might want to report things that didn't
    #     make it, but 1 seems like a reasonable "definitely do not care"
    #     threshold.
    ns_text = miscresults$NOT_SIGNIFICANT,
    check_alpha = 0.05,
    debug = FALSE
) {
    # Reports a two-sample t test, following argument conventions for t.test.
    result <- t.test(
        x = x,
        y = y,
        paired = paired,
        var.equal = var.equal,
        conf.level = conf.level
    )
    if (debug) {
        print(result)
    }
    t <- result$statistic
    t_txt <- miscresults$fmt_float(t)
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
        return(sprintf("%s < %s, %s", t_symbol_df_txt, min_t_txt, ns_text))
    }
    p_txt <- miscresults$mk_p_text_with_label(p, ns_text = ns_text)
    return(sprintf("%s = %s, %s", t_symbol_df_txt, t_txt, p_txt))
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscresults" %in% search()) detach("miscresults")
attach(miscresults)  # subsequent additions not found, so attach at the end
