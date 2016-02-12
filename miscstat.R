# miscstat.R

requireNamespace("lmerTest")
requireNamespace("lsmeans")
requireNamespace("multcomp")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscstat = new.env()

miscstat$MAX_EXPONENT = log(.Machine$double.xmax)
miscstat$VERY_SMALL_NUMBER = 1e-323 # .Machine$double.xmin is 2.2e-308, but this seems to manage.

#==============================================================================
# Working with machine limits
#==============================================================================

miscstat$convert_zero_to_very_small_number <- function(x) {
    # for logs: or log(0) will give -Inf and crash the L-BFGS-B optimzer
    ifelse(x == 0, miscstat$VERY_SMALL_NUMBER, x)
}

miscstat$reset_rng_seed <- function() {
    set.seed(0xbeef)
}

#==============================================================================
# Efficient calculation with extremely small numbers
#==============================================================================

miscstat$log_of_mean_of_numbers_in_log_domain <- function(log_v) {
    # http://stackoverflow.com/questions/7355145/mean-of-very-small-values
    max_log = max(log_v)
    logsum = max_log + log(sum(exp(log_v - max_log)))
    logmean = logsum - log(length(log_v))
    return(logmean)
}

#==============================================================================
# Summary statistics
#==============================================================================

miscstat$sem <- function(x) {
    # won't do anything silly with NA values since var() will return NA in that case
    sqrt(var(x)/length(x))
}

miscstat$half_confidence_interval_t <- function(x, ci = 0.95) {
    n = length(x)
    df = n - 1
    sem = miscstat$sem(x)
    crit_p = 1 - ((1 - ci) / 2) # e.g. 0.975 for ci == 0.95
    crit_t = qt(crit_p, df = df)
    return(crit_t * sem)
    # confidence interval is mean +/- that
}

miscstat$confidence_interval_t <- function(x, ci = 0.95) {
    hci = half_confidence_interval_t(x, ci)
    m = mean(x)
    return(c(m - hci, m + hci))
}

miscstat$summarize_by_factors <- function(data, depvarname, factornames) {
    ddply(
        data,
        factornames,
        function(drow) {
            values = drow[, depvarname]
            c(
                mean = mean(values),
                min = min(values),
                max = max(values),
                sd = sd(values),
                var = var(values),
                sem = miscstat$sem(values)
            )
        }
    )
}

#==============================================================================
# p-values
#==============================================================================

miscstat$sidak_alpha <- function(familywise_alpha, n_comparisons) {
    # returns corrected alpha
    1 - (1 - familywise_alpha) ^ (1 / n_comparisons)
}

miscstat$sidak_familywise_alpha <- function(alpha_per_test, n_comparisons) {
    1 - (1 - alpha_per_test) ^ (n_comparisons)
}

miscstat$sidak_corrected_p <- function(uncorrected_p, n_comparisons) {
    # returns corrected p value
    # http://v8doc.sas.com/sashtml/stat/chap43/sect14.htm
    1 - (1 - uncorrected_p) ^ (n_comparisons)
}

#==============================================================================
# Goodness of fit
#==============================================================================

miscstat$aic <- function(nLL, k) {
    # Akaike Information Criterion
    2 * k + 2 * nLL
    # = 2k - 2ln(L)
}

miscstat$nll_from_aic <- function(aic, k) {
    (aic - 2 * k ) / 2
}

# Strong argument to prefer AICc over AIC:
# http://en.wikipedia.org/wiki/Akaike_information_criterion
# http://www.sortie-nd.org/lme/Statistical%20Papers/Burnham_and_Anderson_2004_Multimodel_Inference.pdf

# k = num_parameters:

miscstat$aicc <- function(nLL, k, n) {
    # Akaike Information Criterion, corrected
    aic(nLL, k) + 2 * k * (k + 1) / (n - k - 1)
}

miscstat$bic <- function(nLL, k, n) {
    # Bayesian Information Criterion
    2 * nLL + k * log(n)
    # ... = -2 ln(L) + k ln(n)
}

miscstat$lr_test <- function(model1_nLL, model1_df, model2_nLL, model2_df) {
    # Ensure df2 > df1
    if (model2_df > model1_df) {
        df1 = model1_df
        df2 = model2_df
        nLL1 = model1_nLL
        nLL2 = model2_nLL
    }
    else {
        df1 = model2_df
        df2 = model1_df
        nLL1 = model2_nLL
        nLL2 = model1_nLL
    }
    # Don't work with exp(-nLL) -- numbers too small, get rounded to zero (see e.g. "exp(-3000)")
    D = 2 * nLL1 - 2 * nLL2
    # D = -2 ln(likelihood for null model) + 2 ln(likelihood for alternative model)
    df = df2 - df1
    p = 1 - pchisq(D, df)
    cat("df1 = ", df1, "\n")
    cat("df2 = ", df2, "\n")
    cat("D (= chi-square) = ", D, "\n")
    cat("df = ", df, "\n")
    cat("p = ", p, "\n")
    return(p)
}

#==============================================================================
# Distributions
#==============================================================================

# AVOID, use PDF instead for MAP
miscstat$p_data_or_more_extreme_from_normal <- function(x, means, sds) {
    ifelse(
        x > means,
        2 * (1 - pnorm(x, means, sds, lower.tail=TRUE) ),
        2 * (1 - pnorm(x, means, sds, lower.tail=FALSE) )
    )
}


#==============================================================================
# softmax function
#==============================================================================

miscstat$softmax <- function(x, b = 1, debug = TRUE) {
    # x: vector of values
    # b: exploration parameter, or inverse temperature [Daw2009], or 1/t where:
    # t: temperature (towards infinity: all actions equally likely; towards zero: probability of action with highest value tends to 1)
    # DO NOT USE TEMPERATURE DIRECTLY: the optimizer may take it to zero, giving an infinity.
    # vector may have NA values in
    # return value: vector of probabilities
    constant = mean(x, na.rm=TRUE)
    products = x * b - constant
    # ... softmax is invariant to addition of a constant: Daw article and http://www.faqs.org/faqs/ai-faq/neural-nets/part2/section-12.html#b
    if (max(products, na.rm=TRUE) > MAX_EXPONENT) {
        if (debug) cat("OVERFLOW in softmax(): x =", x, ", b =", b, ", constant =", constant, ", products=", products, "\n")
        answer = rep(0, length(x))
        answer[which.max(x)] = 1
        answer[is.na(x)] = NA
    }
    else {
        exponented = exp(products)
        answer = exponented / sum(exponented, na.rm=TRUE)
    }
    return(answer)
}

#==============================================================================
# proportion
#==============================================================================

miscstat$proportion_x_from_a_to_b <- function(x, a, b) {
    (1 - x) * a + x * b
}

#==============================================================================
# randomness
#==============================================================================

miscstat$coin <- function(p) {
    return(p > runif(1))
}

miscstat$roulette <- function(p) {
    # p is a vector of probabilities that sum to 1
    # return value: vector of truth values: one TRUE, the rest FALSE, selected according to the probabilities
    n_options = length(p)
    cum_p = cumsum(p)
    r = runif(1) # random variable
    choice = rep(FALSE, n_options)
    choice[cum_p == min(cum_p[cum_p > r])] = TRUE
    return(choice)
}

#==============================================================================
# ANOVA/linear modelling
#==============================================================================

#------------------------------------------------------------------------------
# Diagnostic plots
#------------------------------------------------------------------------------

miscstat$rvfPlot <- function(model, FONTSIZE=10) {
    # https://rpubs.com/therimalaya/43190
    # Note that the other diagnostic plots shown there fail with lme models.
    return (
        ggplot(model, aes(.fitted, .resid))
        + geom_point()
        + stat_smooth(method="loess")
        + geom_hline(yintercept=0, col="red", linetype="dashed")
        + xlab("Fitted values")
        + ylab("Residuals")
        + ggtitle("Residual vs Fitted Plot")
        + theme_classic()
        + theme(
            text=element_text(size=FONTSIZE),
            plot.title=element_text(hjust=0, face="bold")  # left title
        )
    )
}

#------------------------------------------------------------------------------
# Post-hoc analysis; SEDs
#------------------------------------------------------------------------------

miscstat$pairwise_contrasts <- function(
        term, model, alternative=c("two.sided", "less", "greater"),
        DEBUG=FALSE) {
    alternative <- match.arg(alternative)
    # We'd normally do:
    #
    #   multcomp::glht(model, linfct = multcomp::mcp(area = "Tukey"))
    #
    # where "area" is a factor in the model.
    # But this can't do interactions, I don't think. An alternative is:
    #
    #   multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ area)
    #   multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ area:treatment)
    #
    # Some refs:
    #
    #   https://mailman.ucsd.edu/pipermail/ling-r-lang-l/2012-November/000393.html
    #   http://mindingthebrain.blogspot.co.uk/2013/04/multiple-pairwise-comparisons-for.html
    #   http://stats.stackexchange.com/questions/43664/mixed-model-multiple-comparisons-for-interaction-between-continuous-and-categori
    #   http://stats.stackexchange.com/questions/120604/which-post-hoc-is-more-valid-for-multiple-comparison-of-an-unbalanced-lmer-model
    #
    # However, we want the "area" or "area:treatment" thing to come in as a
    # variable. This is all a bit ugly...
    #
    #   http://adv-r.had.co.nz/Computing-on-the-language.html
    #   http://stackoverflow.com/questions/5542945/opposite-of-rs-deparsesubstitutevar
    #
    # Anyway, the answer in R is to eval it.

    expr <- paste(
        "multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ ", term, "), ",
        "alternative=\"", alternative, "\")",
        sep="")
    g <- eval(parse(text=expr))
    summ <- summary(g)
    test <- summ$test
    # test includes:
    #   coefficients = "Estimate"
    #   sigma = "Std. Error" (of the difference) = SED
    #   tstat = "t value"
    #   pvalues = "Pr(>|t|)"
    # ... for each of which, names() gives the tests
    d <- data.frame(
        term = term,
        comparison = names(test$coefficients),
        estimate = test$coefficients,
        sed = test$sigma,
        t = test$tstat,
        p = test$pvalues,
        alternative = alternative
    )
    return(d)
}

miscstat$get_n_for_factor <- function(term, model) {
    factors <- strsplit(term, ":", fixed=TRUE)[[1]]
    d <- model@frame
    n_list <- count(d, factors)$freq
    harmonic_mean_n <- miscmath$harmonic_mean(n_list)
    return(data.frame(
        term=term,
        n_list=paste(n_list, collapse=","),
        harmonic_mean_n=harmonic_mean_n
    ))
}

miscstat$are_predictors_factors <- function(model, predictors) {
    if (length(predictors) < 1) {
    } else if (length(predictors) == 1) {
        # if you use the sapply method with just one, it operate on every
        # row...
        is.factor(model@frame[, predictors])
    } else {
        sapply(model@frame[, predictors], function(col) is.factor(col))
    }
}

miscstat$predictor_names_from_term <- function(term) {
    strsplit(term, ":")[[1]]
}

miscstat$are_all_predictors_in_term_factors <- function(model, term) {
    predictors <- miscstat$predictor_names_from_term(term)
    all(miscstat$are_predictors_factors(model, predictors))
}

miscstat$do_terms_contain_only_factors <- function(model, terms) {
    sapply(
        terms,
        miscstat$are_all_predictors_in_term_factors,
        model=model
    )
}

miscstat$sed_info <- function(
        model, term=NULL,
        alternative=c("two.sided", "less", "greater"), DEBUG=FALSE) {
    # model: an lmer/lmerTest model.
    # term: e.g. "area:manipulation:csvalence"
    alternative <- match.arg(alternative)

    summ <- summary(model)
    an <- anova(model)
    # ... the underlying representation (see "class(an)") is a data frame
    all_terms <- rownames(an)

    # Eliminate things with covariates (continuous predictors) in:
    if (DEBUG) {
        cat("ALL TERMS:", all_terms, "\n")
    }
    useful_terms <- all_terms[
        which(miscstat$do_terms_contain_only_factors(model, all_terms))]
    if (DEBUG) {
        cat("FACTOR-ONLY TERMS:", useful_terms, "\n")
    }

    # Find the highest-order interaction
    n_colons <- lapply(useful_terms, misclang$n_char_occurrences, char=":")
    highest_order_interaction <- useful_terms[which.max(n_colons)]

    # Pairwise contrasts for factors
    comparisons <- ldply(
        useful_terms,
        miscstat$pairwise_contrasts,
        model,
        alternative=alternative
    )

    # std_error_fixed_effects_estimates <- sqrt(diag(vcov(model)))
    # names(std_error_fixed_effects_estimates) <- vcov(model)@Dimnames[[1]]
    #
    # ... Douglas Bates, https://stat.ethz.ch/pipermail/r-help/2006-July/109308.html
    # http://lme4.r-forge.r-project.org/slides/2009-07-16-Munich/Precision-4.pdf
    # NOTE, for example, that the "standard error of effect X" where X is
    # something like "treatment - control" is, obviously, an SED.

    n_by_factor <- ldply(
        useful_terms,
        miscstat$get_n_for_factor,
        model
    )
    an <- cbind(an[useful_terms, ], n_by_factor)
    an <- rename(an, c("Mean Sq"="ms_effect",
                       "F.value"="F",
                       "Pr(>F)"="p"))
    an <- within(an, {
        ms_error <- ms_effect / F  # since F = ms_effect / ms_error
        iffy_sed <- sqrt(2 * ms_error / harmonic_mean_n)
        # t_eq_sqrt_F_for_2_grps <- sqrt(F)
    })
    highest_interaction_iffy_sed = an[highest_order_interaction,
                                        "iffy_sed"]

    # *** Not sure that iffy_sed is always right. OK in simple situations, but
    # not so sure in complex ones...

    return(list(
        pairwise_contrasts = comparisons,
        highest_order_interaction = highest_order_interaction,
        coefficients = summ$coefficients,
        anova = an,
        notes = c(
            "There is no *one* SED appropriate for all comparisons! See e.g. Cardinal & Aitken 2006 p98.",
            "Pairwise contrasts use multcomp::glht(model, linfct = lsmeans::lsm(pairwise ~ FACTOR)).",
            "Least-squares means estimates (with SE of estimate) are from lmerTest::lsmeans().",
            "highest_interaction_iffy_sed is the iffy_sed for the highest_order_interaction term. But see note 1 above."
        ),
        highest_interaction_iffy_sed = highest_interaction_iffy_sed,
        lsmeans = lmerTest::lsmeans(model)
    ))
}

IGNOREME_MISCSTAT_EXAMPLE <- "

# =============================================================================
# WORKING/THINKING
# =============================================================================

testdata <- expand.grid(
    A=c(1, 2, 3),
    B=c(10, 11, 12),
    subj=seq(1,50)
)
testdata <- within(testdata, {
    y <- 13 + 0.5*A + 6*B + rnorm(sd=0.5, n=nrow(testdata))
    # will give intercept = 13 + 0.5*1 + 6*10 = 73.5 (at A1, B1)
    #   A2 effect = 0.5 (relative to A1)
    #   A3 effect = 1   (relative to A1)
    #   B2 effect = 6   (relative to B1)
    #   B3 effect = 12  (relative to B1)
    #   interaction terms = 0
    A <- as.factor(A)
    B <- as.factor(B)
    subj <- as.factor(subj)
})
testmodel <- lmer(y ~ A*B + (1 | subj), data=testdata)
print(sed_info(testmodel))

"

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscstat" %in% search()) detach("miscstat")
attach(miscstat)  # subsequent additions not found, so attach at the end
