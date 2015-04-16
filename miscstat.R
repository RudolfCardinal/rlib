# miscstat.R

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscstat = new.env()

miscstat$MAX_EXPONENT = log(.Machine$double.xmax)
miscstat$VERY_SMALL_NUMBER = 1e-323 # .Machine$double.xmin is 2.2e-308, but this seems to manage.

#===============================================================================
# Working with machine limits
#===============================================================================

miscstat$convert_zero_to_very_small_number <- function(x) {
    # for logs: or log(0) will give -Inf and crash the L-BFGS-B optimzer
    ifelse(x == 0, miscstat$VERY_SMALL_NUMBER, x)
}

miscstat$reset_rng_seed <- function() {
    set.seed(0xbeef)
}

#===============================================================================
# Efficient calculation with extremely small numbers
#===============================================================================

miscstat$log_of_mean_of_numbers_in_log_domain <- function(log_v) {
    # http://stackoverflow.com/questions/7355145/mean-of-very-small-values
    max_log = max(log_v)
    logsum = max_log + log(sum(exp(log_v - max_log)))
    logmean = logsum - log(length(log_v))
    return(logmean)
}

#===============================================================================
# Summary statistics
#===============================================================================

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
    return( c(m - hci, m + hci) )
}

#===============================================================================
# p-values
#===============================================================================

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

#===============================================================================
# Goodness of fit
#===============================================================================

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

#===============================================================================
# Distributions
#===============================================================================

# AVOID, use PDF instead for MAP
miscstat$p_data_or_more_extreme_from_normal <- function(x, means, sds) {
    ifelse(
        x > means,
        2 * (1 - pnorm(x, means, sds, lower.tail=TRUE) ),
        2 * (1 - pnorm(x, means, sds, lower.tail=FALSE) )
    )
}


#===============================================================================
# softmax function
#===============================================================================

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

#===============================================================================
# proportion
#===============================================================================

miscstat$proportion_x_from_a_to_b <- function(x, a, b) {
    (1 - x) * a + x * b
}

#===============================================================================
# randomness
#===============================================================================

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
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscstat" %in% search()) detach("miscstat")
attach(miscstat)  # subsequent additions not found, so attach at the end
