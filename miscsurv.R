# miscsurv.R
#
# Miscellaneous functions for survival analysis.

# =============================================================================
# Packages
# =============================================================================

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        car,  # for car::Anova
        data.table,
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

miscsurv <- new.env()


# =============================================================================
# mk_survfit_stratum_table
# =============================================================================

miscsurv$mk_survfit_stratum_table <- function(survfit_object) {
    # Takes a survfit object, from survival::survfit() or
    # survminer::surv_fit().
    #
    # Creates a table with columns (see ?survfit.object):
    #       stratum
    #       n (in that stratum)
    #       time (the time points, t, at which the curve has a step)
    #       n.risk (number at risk at time t)
    #       n.censor (number exiting the risk set without an event at time t)
    #       surv (estimated proportion surviving at time t+0)
    #       cumhaz (cumulative hazard for each transition = -log(surv))
    #       std.err (standard error of the cumulative hazard) [2]
    #       upper (lower confidence interval for the survival curve) [1]
    #       lower (upper confidence interval for the survival curve) [1]
    #       conf.int (the level of the confidence intervals) [1]
    #
    # Notes:
    #
    # [1] By default the 95% confidence interval, but this is set by the
    # "conf.int" parameter to the function that created the survfit object.
    # Confidence intervals are calculated from standard error via the
    # "survfit_confint" function in
    # https://github.com/therneau/survival/blob/master/R/survfit.R.
    #
    # [2] For Cox proportional hazards, the calculation of std.error and
    # confidence intervals is via coxsurv.fit() in
    # https://github.com/therneau/survival/blob/master/R/coxsurvfit.R, and
    # survfit.coxph() in
    # https://github.com/therneau/survival/blob/master/R/survfit.coxph.R.
    #
    # For a demo "basic" survival object:
    #       library(survival)
    #       fit1 <- survfit(Surv(time, status) ~ x, data = aml)
    #       p1 <- survminer::ggsurvplot(fit1, conf.int = TRUE)
    #
    # A comparison with this function:
    #       library(patchwork)
    #       stratumtable <- miscsurv$mk_survfit_stratum_table(fit1)
    #       p2 <- ggplot(stratumtable, aes(x = time, y = surv, colour = stratum, fill = stratum)) + geom_step(aes(y = lower), linetype = "dotted") + geom_step(aes(y = upper), linetype = "dotted") + geom_step() + geom_point()
    #       # ... note: there isn't a simple geom_step() equivalent of geom_ribbon() yet.
    #       print(p1$plot | p2)
    #
    # For a demo via survminer::surv_fit():
    #       library(survfit)
    #       fit2 <- surv_fit(Surv(time, status) ~ x, data = aml)
    #
    # A version where confidence intervals are NOT 95%:
    #       fit3 <- survfit(Surv(time, status) ~ x, data = aml, conf.int = 0.8)

    sf <- survfit_object  # shorter name internally
    n_strata <- length(sf$strata)
    strata_names <- names(sf$strata)
    strata_lengths <- sf$strata
    d <- NULL
    cum_n <- 0
    for (i in 1:n_strata) {
        start_idx <- cum_n + 1
        end_idx <- cum_n + strata_lengths[i]
        d <- rbind(d, data.table(
            stratum = strata_names[i],
            # Order as in ?survfit.object:
            n = sf$n[i],
            time = sf$time[start_idx : end_idx],
            n.risk = sf$n.risk[start_idx : end_idx],
            n.enter = sf$n.enter[start_idx : end_idx],
            n.censor = sf$n.censor[start_idx : end_idx],
            surv = sf$surv[start_idx : end_idx],
            std.err = sf$std.err[start_idx : end_idx],
            cumhaz = sf$cumhaz[start_idx : end_idx],
            upper = sf$upper[start_idx : end_idx],
            lower = sf$lower[start_idx : end_idx],
            conf.int = sf$conf.int
        ))
        cum_n <- cum_n + strata_lengths[i]
    }
    return(d)
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscsurv" %in% search()) detach("miscsurv")
attach(miscsurv)  # subsequent additions not found, so attach at the end
