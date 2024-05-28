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

miscresults$DEFAULT_SIG_FIG <- 3
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
    sig_fig = miscresults$DEFAULT_SIG_FIG,
    allow_sci_notation = TRUE,
    na_text = "NA"
) {
    # Format a floating-point (real) number, according to a number of
    # significant figures, allowing scientific notation or not. Return values
    # might look like "0.1", "2.2 × 10^−16^" (the latter using ftExtra markup).
    if (is.na(x)) {
        return(na_text)
    }
    if (allow_sci_notation) {
        txt <- as.character(signif(x, digits = sig_fig))
        # May or may not be in scientific notation.
        # If scientific notation:
        scimatch <- stringr::str_match(txt, "(.+)e(-?)(\\d+)")
        if (!is.na(scimatch[1])) {
            # Using scientific notation. Make it pretty!
            radix <- scimatch[2]
            sign <- scimatch[3]
            exponent <- stringr::str_replace(scimatch[4], "^0+" ,"")
            # ... remove leading zeros
            # Use ^...^ for superscript with ftExtra.
            txt <- paste0(radix, " × 10^", sign, exponent, "^")
        }
    } else {
        # https://stackoverflow.com/questions/3245862
        txt <- formatC(
            signif(x, digits = sig_fig),
            digits = sig_fig,
            format = "fg",
            flag = "#"
        )
    }
    # Convert hyphens to proper minus signs:
    HYPHEN <- "-"
    MINUS <- "−"
    txt <- stringr::str_replace_all(txt, HYPHEN, MINUS)
    return(txt)
}


miscresults$mk_p_text <- function(
    p,
    sig_fig = miscresults$DEFAULT_SIG_FIG,
    minimum_shown = miscresults$MINIMUM_P_SHOWN
) {
    # From a p value, return a string such as "*p* = 0.03" or "p < ***". Uses
    # fmt_float().
    if (p < minimum_shown) {
        return(paste0(
            "*p* < ",
            miscresults$fmt_float(minimum_shown, sig_fig = sig_fig)
        ))
    }
    return(paste0(
        "*p* = ",
        miscresults$fmt_float(p, sig_fig = sig_fig)
    ))
}


miscresults$mk_p_text_with_label <- function(
    p,
    sig_fig = miscresults$DEFAULT_SIG_FIG,
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
            sig_fig = sig_fig,
            minimum_shown = minimum_shown
        ),
        miscresults$mk_sig_label(
            p,
            sidak_correction_n = sidak_correction_n_for_asterisks,
            ns_text = ns_text
        )
    ))
}


miscresults$mk_df_text <- function(df) {
    # Format degrees of freedom (df) appropriately -- as an exact integer, or
    # to 1 dp if it is not integer (since knowing the fact of not being an
    # integer is often quite important!).
    if (as.integer(df) == df) {
        return(as.character(df))
    }
    return(sprintf("%.1f", df))
}


miscresults$mk_n_percent <- function(
    n,
    total,
    sig_fig = miscresults$DEFAULT_SIG_FIG
) {
    # Format a number as "n (x%)", where x is the percentage form of n / total.
    pct <- 100 * n / total
    pct_txt <- paste0(
        miscresults$fmt_float(pct, sig_fig = sig_fig, allow_sci_notation = FALSE),
        "%"
    )
    return(sprintf("%d (%s)", n, pct_txt))
}


miscresults$mk_mean_sd <- function(
    x,
    sig_fig = miscresults$DEFAULT_SIG_FIG,
    allow_sci_notation = TRUE,
    na.rm = TRUE,
    with_brackets = TRUE,
    with_plus_minus = TRUE
) {
    # Calculate a mean and standard deviation (SD) from the vector provided, and
    # show this as "μ (± σ)", or "μ ± σ" if with_brackets is FALSE. If
    # with_plus_minus is FALSE, omit "±" (but brackets required).
    if (!with_brackets && !with_plus_minus) {
        stop(
            "Parameters with_brackets and with_plus_minus cannot both be FALSE"
        )
    }
    m <- mean(x, na.rm = na.rm)
    s <- sd(x, na.rm = na.rm)
    m_text <- miscresults$fmt_float(
        m, sig_fig = sig_fig, allow_sci_notation = allow_sci_notation
    )
    s_text <- miscresults$fmt_float(
        s, sig_fig = sig_fig, allow_sci_notation = allow_sci_notation
    )
    if (with_plus_minus) {
        s_group <- paste0("± ", s_text)
    } else {
        s_group <- s_text
    }
    if (with_brackets) {
        return(sprintf("%s (%s)", m_text, s_group))
    } else {
        return(sprintf("%s %s", m_text, s_group))
    }
}


miscresults$mk_mean_ci <- function(
    x,
    ci = 0.95,
    range_text = "–",  # en dash; " to " or ", " are other sensible options
    ci_prefix = "(",
    ci_suffix = ")",
    sig_fig = miscresults$DEFAULT_SIG_FIG,
    allow_sci_notation = TRUE,
    na.rm = TRUE
) {
    # From a vector, show a mean and confidence interval (e.g. 95% CI) as e.g.
    # "μ (a–b)".
    m <- mean(x, na.rm = na.rm)
    ci_pair <- miscstat$confidence_interval_t(x, ci = ci, na.rm = na.rm)
    ci_lower <- ci_pair["ci_lower"]
    ci_upper <- ci_pair["ci_upper"]
    return(paste0(
        miscresults$fmt_float(
            m,
            sig_fig = sig_fig, allow_sci_notation = allow_sci_notation
        ),
        " ",
        ci_prefix,
        miscresults$fmt_float(
            ci_lower,
            sig_fig = sig_fig, allow_sci_notation = allow_sci_notation
        ),
        range_text,
        miscresults$fmt_float(
            ci_upper,
            sig_fig = sig_fig, allow_sci_notation = allow_sci_notation
        ),
        ci_suffix
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
