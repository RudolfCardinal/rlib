#!/usr/bin/env Rscript
#
# test_survival_methods.R
#
# Exploring different approaches to survival analysis.
# Rudolf Cardinal, 14 Dec 2023 onwards.


# =============================================================================
# References
# =============================================================================
#
# [CoxOates1984] Cox, David Roxbee, and D. Oakes. Analysis of Survival Data.
# Monographs on Statistics and Applied Probability 21. Boca Raton, FL: Chapman
# & Hall/CRC, 1984.
# ... seminal monograph.
#
# [Leemis1987] https://doi.org/10.1287/opre.35.6.892
# [Bender2005] PMID 15724232, https://doi.org/10.1002/sim.2059
# [Austin2012] https://pubmed.ncbi.nlm.nih.gov/22763916/
# ... generating survival times
#
# [Pazzagli2017] https://pubmed.ncbi.nlm.nih.gov/29285840/
# ... review on time-varying exposure problems in pharmacoepidemiology.
#
# [Carstensen2023] Carstensen, Bendix. ‘Who Needs the Cox Model Anyway?
# (Version 4)’, November 2023. http://bendixcarstensen.com/WntCma.pdf.
# ... excellent tutorial on regression-based approaches to survival analysis,
# including time-varying risk.


# =============================================================================
# Libraries
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    conflicted,
    data.table,
    flextable,
    Epi,  # for poisreg, glm.Lexis
    mgcv,  # includes s(, for splines
    patchwork,
    rms,  # for vif
    survival,
    survminer,
    tidyverse  # for ggplot
)

# RLIB_PREFIX <- "/srv/cardinal_rlib/"
RLIB_PREFIX <- ""  # for internal testing
source(paste0(RLIB_PREFIX, "debugfunc.R"))
source(paste0(RLIB_PREFIX, "miscmath.R"))
source(paste0(RLIB_PREFIX, "miscresults.R"))
source(paste0(RLIB_PREFIX, "miscstat.R"))
source(paste0(RLIB_PREFIX, "miscsurv.R"))

# Set up flextable defaults before ANY functions using flextable formatting.
flextable::set_flextable_defaults(
    font.family = "Arial",
    font.size = 10,
    border.color = "gray",
    digits = 3,  # usually significant figures
    big.mark = ",",  # thousands separator
    na_str = "NA",
    nan_str = "NaN"
)


# =============================================================================
# Constants
# =============================================================================

N_SUBJECTS <- 1e3
SEED <- 1
CENSOR <- TRUE
MAX_DURATION <- 5  # if CENSOR is true
DEBUG <- TRUE

# Our generative risk model.

# We'd like a "half-life" specification so I understand this.
HALF_LIFE <- 2  # λ
# The Cox PH model has ([Bender2005], eq. 2):
#       survival S(t | x) = exp[-H0(t) exp(β'x)]
# i.e. for no additional coefficient,
#       survival S(t | x) = exp[-H0(t)]
# Half-life λ is given by
#       0.5 = exp[-H0(λ)]
#       ln(0.5) = -H0(λ)
#       H0(λ) = -ln(0.5) = ln(2)
# So I think what we're getting at is this:
BASELINE_HAZARD <- log(2) / HALF_LIFE
# Yes, this is spot on when we try some test analyses.
BASELINE_BETA <- log(BASELINE_HAZARD)

# β; one unit increase in x multiplies hazard rate by exp(βx).
# So for a doubling of hazard for x = 1, use β = ln(2) ≈ 0.69.
X_HAZARD_MULTIPLE <- 2
X_HAZARD_BETA <- log(X_HAZARD_MULTIPLE)


# =============================================================================
# Helper functions: significant
# =============================================================================

mksurvplot <- function(
    formula,
    data,
    title,
    line_alpha = 0.5,
    line_colour = "grey",
    vlines = 1:4,
    hlines = 0.5 ^ seq(1:4)
) {
    # Make a survival plot.
    #
    # Args:
    #   formula, data
    #       Formula and data for survminer::surv_fit().
    #   title, line_alpha, line_colour
    #       Cosmetics
    #   vlines, hlines
    #       Vertical/horizontal lines to add.
    #
    # Note:
    #   test_formula <- survobject ~ predictors
    #   test_coxph_model <- coxph(test_formula, data = d)
    #   test_survfit_1 <- survfit(test_coxph_model, data = d)
    #   test_survfit_2 <- survfit(test_coxph_model)  # SAME AS #1
    #   test_survfit_3 <- survfit(test_formula, data = d)  # DIFFERENT
    #   # test_survfit_4 <- survfit(test_formula)  # can't work
    #
    # Therefore
    #   ggsurvplot(survfit(test_coxph_model), data = d)
    # and
    #   ggsurvplot(survfit(test_formula, data = d), data = d)
    # are different, and it's the second that splits the graph by predictors
    # properly.

    p <- survminer::ggsurvplot(
        survminer::surv_fit(formula, data = data),
        data = data,
        risk.table = TRUE,
        conf.int = TRUE
    )
    # The ggplot component is in the "$plot" member;
    # https://stackoverflow.com/questions/59951004 But does that perhaps not
    # include the risk table? Modify, rather than just returning the ggplot
    # part. Yes, that's right. Note that there is also the "$table" member if
    # you add a risk table; that is also a ggplot object.
    p$plot <- (
        p$plot
        + ggtitle(title)
        + geom_vline(
            xintercept = vlines,
            colour = line_colour,
            alpha = line_alpha
        )
        + geom_hline(
            yintercept = hlines,
            colour = line_colour,
            alpha = line_alpha
        )
    )
    return(p)
}




build_composite_survival_prediction <- function(
    model,
    newtime,
    alpha = 0.05,
    ...
) {
    # From a GLM model ("model") of survival, e.g.
    #       glm(
    #           cbind(n_died, observation_time) ~ 1 + x,
    #           family = poisreg,
    #           data = d1
    #       )
    # produce predicted survival (with confidence intervals), predicting at
    # the times in "newtime". The value "alpha" is for a two-tailed confidence
    # interval.
    #
    # The input "..." contains other predictors to crosscombine and use, for
    # each time point.
    #
    # The resulting table has these columns:
    #   t
    #   ... other predictors ...
    #   survival
    #   survival_lower
    #   survival_upper
    #
    # RATIONALE: The glm.Lexis() function works fine but seems a bit risky in
    # terms of predicting things later; I produced duff predictions (not
    # reading [all] input values correctly). An alternative is to use glm()
    # with the "poisreg" family. This expects a dependent variable of the form
    # cbind(numevents, time). But actually it's the Epi::ci.surv() function
    # that I've not got the hang of; this seems to ignore its inputs. How is it
    # operating?
    # - ci.surv() calls ci.cum()
    # - It looks like that takes the first column name as the "time" variable:
    #           tnam <- colnames(ctr.mat)[1]
    # - So I think these functions only predict with respect to time -- e.g. p8
    #   of Carstensen (2023); also p13-14.
    # - Or, only for a single strand of time at specific values of other
    #   covariates. So we have to do multiple chunks to build up a data frame.

    time_for_prediction <- data.frame(t = newtime)
    other_predictors <- expand.grid(...)
    alldata <- NULL
    n_other_predictor_combinations <- nrow(other_predictors)
    if (n_other_predictor_combinations == 0) {
        alldata <- cbind(
            time_for_prediction,
            Epi::ci.surv(
                obj = model,
                ctr.mat = time_for_prediction,
                alpha = alpha
            )
        )
    } else {
        for (i in 1:n_other_predictor_combinations) {
            current_row <- other_predictors[i, ]
            names(current_row) <- colnames(other_predictors)
            # Named args for transform():
            args <- append(
                list("_data" = time_for_prediction),
                as.list(current_row)
            )
            current_predictors <- do.call("transform", args)
            newchunk <- cbind(
                current_predictors,
                Epi::ci.surv(obj = model, ctr.mat = current_predictors)
            )
            alldata <- rbind(alldata, newchunk)
        }
    }
    lower_colname <- paste0(100 * alpha / 2, "%", sep = "")
    upper_colname <- paste0(100 * (1 - alpha / 2), "%", sep = "")
    renamecols <- c(
        survival = "Estimate",
        survival_lower = lower_colname,
        survival_upper = upper_colname
    )
    predicted <- (
        alldata
        %>% rename(!!!renamecols)
        %>% as.data.table()
    )
    return(predicted)
}


# =============================================================================
# Helper functions: trivial/display
# =============================================================================

print_heading <- function(x) {
    line <- paste(c(rep("=", 79), "\n"), collapse = "")
    cat(line, ">>> ", x, "\n", line, sep = "")
}


manual_check_internal <- function(label, varname, varvalue) {
    cat(
        "+++ Compare ", label, " with expected value of ", varname, " = ",
        varvalue, "\n", sep = ""
    )
}
manual_check_x_multiple <- function(label) {
    manual_check_internal(label, "X_HAZARD_MULTIPLE", X_HAZARD_MULTIPLE)
}
manual_check_x_beta <- function(label) {
    manual_check_internal(label, "X_HAZARD_BETA", X_HAZARD_BETA)
}
manual_check_baseline_multiple <- function(label) {
    manual_check_internal(label, "BASELINE_HAZARD", BASELINE_HAZARD)
}
manual_check_baseline_beta <- function(label) {
    manual_check_internal(label, "BASELINE_BETA", BASELINE_BETA)
}


# =============================================================================
# Helper functions: data synthesis
# =============================================================================

mk_survival_time <- function(hazard_rate, scale = 1) {
    # Generate specimen times to event, one for each value of hazard_rate,
    # using the relevant hazard rate (scaled by "scale"). This is a STOCHASTIC
    # function; e.g. try mk_survival_time(rep(1, 100)).
    #
    # See
    # - [Austin2012]
    # - [Leemis1987]
    # - [Bender2005] -- particularly this one.
    #
    # But note also, from [CoxOates1984], chapter 2:
    # - The hazard rate is the expected number of events per unit time.
    # - T is the failure time, and t is continuous time.
    # - F(t) is the probability of surviving to time t, i.e. P(T ≥ t);
    #   F(0) = 1.
    # - f(t) is the probability "atom" at time t. Is this the probability
    #   density function? Or is that h(t), as below? The function f(t) is
    #   defined as -F'(t), the negative derivative of F(t).
    # - F(t) is the integral of f().
    # - h(t) = f(t)/F(t); this is the hazard function.
    # - H(t) is the integrated hazard up to time t.
    # - F(t) = exp[-H(t)]; see page 14.
    # - For discrete distributions (discrete time), F(t) = Product[1 - h_j] for
    #   all j < t; that is, to have T ≥ t, it is necessary and sufficient to
    #   survive all points of support before t. (In other words: h_t is the
    #   point probability of failure in this setting.)
    #
    # Then, separately:
    # - Imagine discrete time with unit step 1 and a constant probability of
    #   failure h. Start off n subjects. What's the distribution of failure
    #   times?
    # - The fraction expected to fail in the first time period is h. The fraction
    #   expected to fail in the second time period is h * fraction_still_going =
    #   h * (1 - h). The fraction expected to fail in timestep t is therefore h *
    #   (1 - h)^(t - 1).
    # - The cumulative number of failures is therefore
    #   t = 0           0
    #   t = 1           h * (1 - h) ^ 0
    #   t = 2           h * (1 - h) ^ 0 + h * (1 - h) ^ 1
    #   t = 3           h * (1 - h) ^ 0 + h * (1 - h) ^ 1 + h * (1 - h) ^ 2
    #   t = n           sum(q = 1...t)[ h * (1 - h)^(q - 1) ]
    #
    # On pages 16-18, [CoxOates1984] consider the exponential distribution,
    # which gives constant hazard.
    #
    # [Bender2005]: "The translation of the regression coefficients from
    # hazard to survival time is easy if the baseline hazard function is
    # constant, i.e. the survival times are exponentially distributed."
    #
    # [Bender2005] derives the survival times T:
    #       survival S(t | x) = exp[-H0(t) exp(β'x)]
    #
    #       H0(t) = integral(from 0 to t){ h0(...) }
    #       ... where h0(t) is the instantaneous baseline hazard function
    #
    #       uniform random variable U ~ Uniform[0, 1]
    #
    #       U = exp[-H0(T) exp(β'x)]
    #
    #       then working not shown:
    #           log(U) = -H0(T) exp(β'x)
    #           log(U) / exp(β'x) = -H0(T)
    #           log(U) exp(-β'x) = -H0(T)
    #           -log(U) exp(-β'x) = H0(T)
    #       leading to:
    #
    #       survival time T = inv_H0[-log(U) exp(-β'x)]
    #
    # ... and "[i]t is just required to insert the inverse of an appropriate
    # cumulative baseline hazard function into [that equation]."
    #
    # RNC: So if the baseline hazard is constant, what's the inverse cumulative
    # baseline hazard function? Presumably the cumulative baseline function
    # for a constant hazard h is just ht, i.e. H0(t) = ht [= H]. The inverse
    # gives time from cumulative hazard, and that would be t = inv_H0(H) = H/h.
    # If that's right, then
    #       survival time T = inv_H0[-log(U) exp(-β'x)]
    #                       = [-log(U) exp(-β'x)] / h
    #
    # RNC: But alternatively, we would like to work with the "net" hazard rate.
    # The overall (net) hazard rate (Bender 2005 eq.1) is:
    #       h(t | x) = h0(t) exp(β'x)
    #
    # Leemis (1987) provides the simple case:
    #       T = invH[-log(U)]           (bottom right of first page)
    #       H(t) = λt                   (in Conclusions)
    # so
    #       invH[H] = H/λ
    # So I think this is correct:
    #       T = -log(u) / (scale * hazard_rate)
    # Meh. Let's do this empirically.

    n <- length(hazard_rate)  # number of values we're calculating for
    u <- runif(n = n, min = 0, max = 1)  # a flat probability density
    T <- -log(u) / (scale * hazard_rate)  # failure time
    return(T)
}


mk_censor_time <- function(n, max_duration = MAX_DURATION) {
    # Generate n random times of censorship, in the range [0, max_duration].
    runif(n = n, min = 0, max = max_duration)
}


add_time_and_event_vars <- function(d, censor = CENSOR) {
    # Modify the data table of specimen data based on the "hazard" column,
    # adding "observation_time" and "died" (and a working variable:
    # "censor_time", which might be beyond "observation_time" if the subject
    # dies before censoring).
    d[, survival_time := mk_survival_time(hazard)]  # a.k.a. failure time
    if (censor) {
        # Some artificial right-censoring:
        d[, censor_time := mk_censor_time(nrow(d))]
        d[, observation_time := pmin(survival_time, censor_time)]  # time for survival
        d[, died := as.integer(survival_time <= censor_time)]  # event 0/1
    } else {
        d[, censor_time := survival_time]
        d[, observation_time := survival_time]
        d[, died := 1]
    }
}


# =============================================================================
# Create data
# =============================================================================

stopifnot(N_SUBJECTS %% 2 == 0)
set.seed(SEED)  # seed random number generator

# bd:, base data
bd <- data.table(subject = paste0("s", 1 : N_SUBJECTS))
bd[, x := rep(c(0, 1), N_SUBJECTS / 2)]

# Our generative risk models:

# NOT THESE!
#       duration := as.integer(runif(n = N_SUBJECTS, min = 0, max = MAX_DURATION))
#       t := duration]  # synonym
#       risk_logit := (BASELINE_HAZARD + X_HAZARD_BETA * x) * t
#       risk_logit := (BASELINE_HAZARD * exp(X_HAZARD_BETA * x)) * t
#   followed by
#       risk_p := miscmath$probability_from_log_odds(risk_logit)
# ... wrong because time is the CONSEQUENCE, not a generator variable.
# How do to it properly: [Austin2012].

# This one follows Cox PH assumptions, I think:
d1 <- copy(bd)
d1[, hazard := BASELINE_HAZARD * exp(X_HAZARD_BETA * x)]
add_time_and_event_vars(d1)
if (DEBUG) {
    print_heading("Data d1:")
    print(d1)
}

# For a later test, we'll also add something highly correlated with x, but
# also binary:

to_binary <- function(x) {
    # Forces to 0/1, with 0.5 being the split point.
    # Use floor(x + 0.5), not round(x); the latter rounds e.g. 0.5 to 0.
    pmin(pmax(0, floor(x + 0.5)), 1)
}
d1[, correl_with_x := to_binary(x + rnorm(n = length(x), mean = 0, sd = 0.3))]
# print(cor(d1$x, d1$correl_with_x))  # high
d1[, copy_of_x := x]


# =============================================================================
# Model d1
# =============================================================================

# -----------------------------------------------------------------------------
# Analyse via logistic regression (WRONG):
# -----------------------------------------------------------------------------

# regm1wrong <- glm(
#     died ~ 0 + observation_time + x : observation_time,
#     # 0 for no additional intercept term; everything is time-dependent
#     family = binomial,
#     data = d1
# )
# print_heading("Data d1, logistic regression model (WRONG):")
# print(summary(regm1wrong))

# -----------------------------------------------------------------------------
# Analyse via Cox proportional hazards model:
# -----------------------------------------------------------------------------

print_heading("Data d1, Cox proportional hazards model (CORRECT):")
surv1 <- survival::Surv(
    time = d1$observation_time,  # for right-censored data, the follow-up time
    event = d1$died,
    type = "right"  # right-censored data; this is the default
)
formula1 <- surv1 ~ x
survfit1a <- survival::survfit(formula1, data = d1)  # ?survfit.object
survfit1b <- survminer::surv_fit(formula1, data = d1)
survfittable1 <- miscsurv$mk_survfit_stratum_table(survfit1b)
survfittable1[, x := as.integer(str_remove(stratum, "x="))]
survfittable1[, x_factor := as.factor(x)]
cph1 <- survival::coxph(formula1, data = d1)
print(cph1)
manual_check_x_beta("'coef' for 'x'")
manual_check_x_multiple("'exp(coef)' for 'x'")
print(summary(cph1))
manual_check_x_multiple("'exp(coef)' for 'x'")

# -----------------------------------------------------------------------------
# Analyse via Poisson regression:
# -----------------------------------------------------------------------------
#
# Can we extract the baseline hazard?
# https://stats.stackexchange.com/questions/68737/ says: Cox models estimate
# hazard ratios *without* having to estimate the baseline hazard function.
# Both a strength and a weakness.
# ... note [Carstensen2023] phrasing: the Cox model operates "leaving the
# baseline partial hazard λ0 unspecified."
#
# Additional models are possible, e.g. Weibull;
# https://www.stata.com/bookstore/flexible-parametric-survival-analysis-stata/.
#
# For more material, see:
# - https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# - https://rviews.rstudio.com/2022/09/06/deep-survival/
# ... and the second of these captures my intuition of using "time" and "event
# during that time?", but does it properly.
#
# So here's how to do it:
# (a) split time into multiple chunks -- see e.g. popEpi::splitMulti
# (b) glm using the "poisreg" family
# ... but complex and ?very slow in practice.
# Or maybe not so complex: Carstensen (2023) goes through this.

print_heading("Data d1, Poisson regression model:")
d1glm <- glm(
    cbind(died, observation_time) ~ 1 + x,
    family = poisreg,
    data = d1
)
print(summary(d1glm))
manual_check_baseline_beta("'Estimate' for '(Intercept)'")
manual_check_x_beta("'Estimate' for 'x'")
print(Epi::ci.exp(d1glm))
manual_check_baseline_multiple("'exp(Est.)' for '(Intercept)'")
manual_check_x_multiple("'exp(Est.)' for 'x'")
d1glmpredicted <- build_composite_survival_prediction(
    d1glm,
    newtime = seq(0, 5, 0.1),
    x = c(0, 1)
)
d1glmpredicted[, x_factor := as.factor(x)]

if (FALSE) {
    d1lexis <- Epi::Lexis(
        exit = list(tfe = observation_time),  # tfe: time from entry?
        exit.status = factor(died, levels = c(0, 1), labels = c("Alive", "Dead")),
        data = d1[, .(subject, x, observation_time, died)]
    )

    mregplain <- Epi::glm.Lexis(
        d1lexis,
        formula = ~ 1 + x
    )
    print(summary(mregplain))
    print(Epi::ci.exp(mregplain))

    d1split <- Epi::splitLexis(d1lexis, seq(0, max(d1lexis$observation_time), 1))
    kn <- c(0, 2.5, 5)  # random
    mregsplit <- Epi::glm.Lexis(
        d1split,
        formula = ~ Epi::Ns(tfe, knots = kn) + x
    )
    print(summary(mregsplit))
    manual_check_x_beta("'Estimate' for 'x'")
    print(Epi::ci.exp(mregsplit))  # parameters and confidence intervals after exponentiation
    manual_check_x_multiple("'exp(Est.)' for 'x'")

    newdataepi <- expand.grid(tfe = seq(0, 0.2, 0.1), x = as.factor(c(0, 1)))
    newdataepi <- (
        #                 +++ THIS IS WRONG: ++++++++++++++++++++++++++++++++
        cbind(newdataepi, Epi::ci.surv(obj = mregplain, ctr.mat = newdataepi))
        %>% rename(
            survival = "Estimate",
            survival_lower = "2.5%",
            survival_upper = "97.5%"
        )
        %>% mutate(x_factor = as.character(x))
        %>% as.data.table()
    )
}


# =============================================================================
# Visualize
# =============================================================================
# There is also Epi::matshade, but fairly complex (and matplot-based).

p1 <- (
    mksurvplot(formula1, d1, "Data 1")
)

line_colour_trad <- "black"
line_width_trad_main <- 1
line_width_trad_small <- 0.5
censor_line_y_length <- 0.005 * 1  # 0.5% of scale
censor_line_x_length <- 0.005 * diff(range(d1glmpredicted$t))  # 0.5% of scale
censor_line_alpha <- 0.5

p2 <- (
    # Predicted:
    ggplot()
    + geom_ribbon(
        data = d1glmpredicted,
        aes(
            x = t, ymin = survival_lower, ymax = survival_upper,
            colour = x_factor, fill = x_factor, group = x_factor
        ),
        alpha = 0.5
    )
    + geom_line(
        data = d1glmpredicted,
        aes(
            x = t, y = survival,
            colour = x_factor, group = x_factor
        )
    )
    # Add a traditional survival curve:
    + geom_line(
        # Survival curve
        data = survfittable1,
        aes(x = time, y = surv, group = x_factor),
        colour = line_colour_trad,
        lwd = line_width_trad_main
    )
    + geom_segment(
        # Censor marks
        data = survfittable1[n.censor > 0],
        aes(
            # Could do this +/-, or +, or diagonal... Could also indicate
            # multiple censoring by length, which is perhaps clearer than many
            # conventions.
            x = time,
            xend = time + censor_line_x_length * n.censor,
            y = surv,
            yend = surv + censor_line_y_length * n.censor,
            group = x_factor
        ),
        colour = line_colour_trad,
        lwd = line_width_trad_small,
        alpha = censor_line_alpha
    )
    + geom_line(
        # Lower confidence interval
        data = survfittable1,
        aes(x = time, y = lower, group = x_factor),
        colour = line_colour_trad,
        lwd = line_width_trad_small
    )
    + geom_line(
        # Upper confidence interval
        data = survfittable1,
        aes(x = time, y = upper, group = x_factor),
        colour = line_colour_trad,
        lwd = line_width_trad_small
    )
    # Cosmetics:
    + theme_bw()
    + ggtitle('Via glm(..., family = poisreg) and Epi::ci.surv()')
)

combinedplot <- (
    (
        (
            p1$plot
            + ggtitle('Via coxph() and ggsurvplot()')
        )
        | p2
    )
    / (
        p1$table
        | p1$table
    )
    + plot_layout(heights = c(10, 1))
)
print(combinedplot)


# =============================================================================
# Correlated predictors
# =============================================================================

cph_correl_pred_4 <- survival::coxph(surv1 ~ correl_with_x + x, data = d1)
# ... separate as we use it in another demo below.

if (FALSE) {
    # surv1 created above

    cph_correl_pred_1 <- survival::coxph(surv1 ~ x, data = d1)
    print(cph_correl_pred_1)
    # x: coef=0.68713, p<2e-16

    cph_correl_pred_2 <- survival::coxph(surv1 ~ correl_with_x, data = d1)
    print(cph_correl_pred_2)
    # correl_with_x: coef=0.60214, p=4.8e-13 (with seed as above)

    cph_correl_pred_3 <- survival::coxph(surv1 ~ x + correl_with_x, data = d1)
    print(cph_correl_pred_3)
    # x: coef=0.71321, p=9.86e-5; correl_with_x: coef=-0.02925, p=0.873

    print(cph_correl_pred_4)
    # correl_with_x: coef=-0.02925, p=0.873; x: coef=0.71321, p=9.86e-5

    cph_correl_pred_5 <- survival::coxph(surv1 ~ x + copy_of_x, data = d1)
    print(cph_correl_pred_5)
    # x: coef=0.68713, p<2e-16; copy_of_x: coef=NA, p=NA

    # Note:
    # It is ORDER-INDEPENDENT.
    # This is the problem of MULTICOLLINEARITY.
    # - Excellent explanation:
    #   https://www.graphpad.com/guides/prism/latest/statistics/stat_cox_reg_results_multicollinearity.htm
    # - Some discussion about the approach, and model comparison:
    #   https://stats.stackexchange.com/questions/248935/correlated-variables-in-cox-model-which-one-is-best
    #   ... obviously an important approach in practice.
    # - Testing for it:
    #   https://stackoverflow.com/questions/23518075/testing-multicollinearity-in-cox-proportional-hazards-using-r
    rms::vif(cph_correl_pred_1)  # 1, as expected
    rms::vif(cph_correl_pred_3)  # 4.8 and 4.8
    car::vif(cph_correl_pred_3)  # 4.8 and 4.8; also gives warning
    # - Discussion of interpretation of variance inflation factors (VIFs):
    #   https://en.wikipedia.org/wiki/Variance_inflation_factor
}


# =============================================================================
# Complex models, for formatting tests
# =============================================================================

complex_n_per_group <- 50
complex_n <- complex_n_per_group * 6
data_complex <- data.table(
    xlinear = rnorm(n = complex_n, mean = 0.3, sd = 1),
    yboolean = as.logical(rbinom(n = complex_n, size = 1, prob = 0.5)),
    abcfactor = rep(c("A", "B", "C"), each = complex_n_per_group * 2),
    pqfactor = rep(c("P", "Q"), each = complex_n_per_group, times = 3)
)
data_complex[, hazard := (
    0.3
    + 0.2 * xlinear
    - 0.1 * as.numeric(yboolean)
    + case_when(
        abcfactor == "A" ~ 0.2,
        abcfactor == "B" ~ -0.2,
        abcfactor == "C" ~ 0,
        .default = NA
    )
    + case_when(
        pqfactor == "P" ~ 0,
        pqfactor == "Q" ~ -0.7,
        .default = NA
    )
)]
add_time_and_event_vars(data_complex)
surv_complex <- survival::Surv(
    time = data_complex$observation_time,  # for right-censored data, the follow-up time
    event = data_complex$died,
    type = "right"  # right-censored data; this is the default
)
cph_testbool  <- survival::coxph(
    surv_complex ~ yboolean,
    data = data_complex
)
cph_complex <- survival::coxph(
    surv_complex ~ xlinear + yboolean + abcfactor + pqfactor,
    data = data_complex
)
cph_complex2 <- survival::coxph(
    surv_complex ~ xlinear * yboolean * abcfactor * pqfactor,
    data = data_complex
)

# =============================================================================
# Flextable output
# =============================================================================

cat("- Onto flextables...\n")
cph_testbool_formatted <- miscresults$mk_cph_table(cph_testbool)
cph_correl_pred_4_formatted <- miscresults$mk_cph_table(cph_correl_pred_4)
cph_complex_formatted <- miscresults$mk_cph_table(
    cph_complex,
    include_factor_reference_levels = TRUE,
    include_logical_reference_levels = TRUE
)
cph_complex2_formatted <- miscresults$mk_cph_table(
    cph_complex2,
    include_factor_reference_levels = TRUE,
    include_logical_reference_levels = FALSE
)
multicph_summary <- miscresults$summarize_multiple_cph(
    list(
        "CPH one" = cph_complex2_formatted,
        "Cox #2" = cph_complex2_formatted,
        "Cox #3" = cph_complex2_formatted
    ),
    correct_alpha_for = "none",
    alpha_correction_method = "sidak"
)

PROMPT <- "Press [Enter] to see next table..."
readline(PROMPT); print(cph_correl_pred_4_formatted$table_flex)
readline(PROMPT); print(cph_testbool_formatted$table_flex)
readline(PROMPT); print(cph_complex_formatted$table_flex)
readline(PROMPT); print(cph_complex2_formatted$table_flex)
readline(PROMPT); print(multicph_summary$table_flex)
