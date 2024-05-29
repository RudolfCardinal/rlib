#!/usr/bin/env Rscript

# =============================================================================
# Notes
# =============================================================================

NOTATION <- "

~
    Denotes 'drawn from' (a distribution).

N(μ, σ^2) or N(μ, σ)
    Normal distribution with mean μ and standard deviation σ (variance σ^2).
    Note that many normal distribution implementations use σ as the parameter,
    not variance [e.g. Stan's normal() and normal_lpdf(); R's rnorm().] We'll
    use N(μ, σ) here.

    In R, to draw random variables, the functions have an 'r' prefix, e.g.
    rnorm(n, mean, sd) for the normal distribution.

    In Stan, use x ~ normal(mean, sd), or target += normal_lpdf(x | mean, sd).

N(0, 1)
    Standard normal distribution. A random variable (RV) with this distribution
    is traditionally denoted Z, and a single value denoted z.

f(x) = d
    Generic probability density function, or simply density function.

    In R, (probability) density functions take a 'd' prefix, e.g. dnorm(x,
    mean, sd) for the normal distribution.

    In Stan, the log density function is e.g. normal_lpdf(x | mean, sd).

F(x) = P(X ≤ x) = p
    Generic cumulative distribution function, transforming a value (quantile)
    of a random variable to the probability that the RV is up to or including
    that value. It's the integral of f(x) from −∞ to x. The naming follows e.g.
    'quartile' (the first quartile corresponds to p = 0.25, etc.).

    Formally, e.g. https://en.wikipedia.org/wiki/Quantile, one can say that x
    is a k-th q-quantile for a variable X if P(X < x) ≤ k/q and P(X ≤ x) ≥ k/q.
    Or, in terms of probability, you can speak of 'p-quantiles' using
    probabilities 0 ≤ p ≤ 1, in which p replaces k/q. So one could say that
    e.g. 0 is the 'p = 0.5 quantile' for the standard normal distribution.

    R refers to these as simply 'distribution' functions, and they take a 'p'
    prefix, so CDF for the normal distribution is p = pnorm(q, mean, sd) for a
    quantile q.

    In Stan, the cumulative distribution is e.g. normal_cdf(x, mean, sd), and
    the log version normal_lcdf(). There are some special versions for some
    distributions, e.g. std_normal_cdf().

Φ(z)
    Cumulative distribution function (CDF) of the standard normal distribution,
    N(0, 1). Transforms a score z, range [−∞, +∞] to a probability, P(Z ≤ z),
    range [0, 1]. Φ() is also called the inverse probit function (see below).

    If Z has a standard normal distribution, then Φ(Z) has a uniform
    distribution from 0 to 1.

    In R, this is pnorm(z).

    In Stan, this is e.g. Phi_approx(z).

Q(p) = F^{-1}(p) = q
    Generic quantile function, or inverse CDF. Maps a probability to the
    corresponding quantile; e.g. Q(0.25) gives the first quartile, and Q(0.01)
    gives the first centile. Formally, a quantile function Q() maps cumulative
    probability to the corresponding value of the random variable; F(x) = P(X ≤
    x) = p, and Q(p) = x.

    In R, quantile functions (inverse CDFs) take a 'q' prefix.

    In Stan, at least one has a '_qf' suffix: std_normal_qf(), as below.
    However, this is not routinely implemented for most distributions.

probit(p)
    The probit function is the quantile function associated with the standard
    normal distribution.  So it is the inverse of the CDF of the standard
    normal distribution, Φ(z), such that probit(Φ(z)) = z and Φ(probit(p)) = p.
    The probit function maps the range [0, 1] to the range [−∞, +∞].

    In R, the probit function is qnorm(p, mean = 0, sd = 1).

    In Stan, this is inv_Phi(p), or std_normal_qf(x). In the help, I think Stan
    uses 'quantile' to mean the associated probability. This is a bit
    confusing. See
    https://mc-stan.org/docs/functions-reference/normal-distribution.html.

THEREFORE, to transform between two distributions, and specifically to
transform from N(0, 1) to anything else: the intermediate currency can be p.
Such as:

    z = rnorm(n = N)        # z ~ N(0, 1)
    p = pnorm(z)            # p = Φ(z)
    b = qbeta(p, 1.2, 1.2)  # b ~ Beta(1.2, 1.2)

This works extremely well in R, as compared to rbeta(n = N, 1.2, 1.2) directly.

So we can use the same process for a modified variable on the scale of z.

However, we may get stuck if Stan lacks a qbeta() equivalent.
Do we need to write one? It's one of those slightly nightmarish algorithms:
https://github.com/SurajGupta/r-source/blob/master/src/nmath/qbeta.c
Oh well -- done.

"


# =============================================================================
# Libraries
# =============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    ggtext,
    patchwork,
    tidyverse
)
RLIB_PREFIX <- "/srv/cardinal_rlib/"
source(paste0(RLIB_PREFIX, "miscfile.R"))


# =============================================================================
# RNG
# =============================================================================

set.seed(1234)


# =============================================================================
# Constants
# =============================================================================

SCRIPT_DIR <- miscfile$current_script_directory()
OUTPUT_PNG <- file.path(SCRIPT_DIR, "_explore_priors.png")

N_VALUES <- 1e6  # use 1e6 for sketch, 5e6 for medium, 1e7 for high accuracy

PRIOR_BETADIST_SHAPE1 <- 1.2  # den Ouden 2013: Beta(1.2, 1.2)
PRIOR_BETADIST_SHAPE2 <- 1.2  # den Ouden 2013
# plot(function(x) dbeta(x, shape1 = 1.2, shape2 = 1.2))

PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1 <- 0.05  # Kanen 2019, Table 2
# Try 0 for reassurance and 5 for a large value -- it's different from the
# underlying Beta(1.2, 1.2) distribution, but reassuringly little.

AHN2017_SD_CAUCHY_SCALE <- 5
ROMEU2020_SD_HALFNORMAL_SD <- 0.2

LINEWIDTH <- 1
KERNEL_DENSITY_N <- 2048  # default 512


# =============================================================================
# Basic maths
# =============================================================================

clip <- function(x, lower = -Inf, upper = +Inf) {
    # Returns the value, clipped to the range
    ifelse(x < lower, lower, ifelse(x > upper, upper, x))
}

restrict <- function(x, lower = -Inf, upper = +Inf) {
    # Returns the value, or NA if out of range.
    ifelse(x < lower, NA, ifelse(x > upper, NA, x))
}


# =============================================================================
# Standard distribution functions
# =============================================================================

std_normal <- function(n) {
    rnorm(n, mean = 0, sd = 1)
}

halfnormal <- function(n, mean = 0, sd = 1) {
    # |N(0, σ)| gives a half-normal positive distribution, but abs() will not
    # do the same for distributions with μ ≠ 0.
    abs(rnorm(n, mean = 0, sd = sd)) + mean
}

halfcauchy <- function(n, location = 0, scale = 1) {
    # Similarly, abs() is valid for location = 0.
    # Just to check R/Stan equivalence:
    #   R: rcauchy(n, location, scale)
    #   Stan: cauchy(location mu, scale sigma)
    # ... same notation. And only one at
    # https://en.wikipedia.org/wiki/Cauchy_distribution. And the half-Cauchy
    # is as you'd expect:
    # https://distribution-explorer.github.io/continuous/halfcauchy.html, which
    # even has an example using abs().
    #
    # Explore Cauchy:
    #       plot(function(x) dcauchy(x, location = 0, scale = 5), -5, 5)
    abs(rcauchy(n, location = 0, scale = scale)) + location
}

phi <- function(x) {
    # Cumulative distribution function (in R terminology: distribution
    # function) of the standard normal distribution, Φ(). It maps [−∞, +∞] to
    # [0, 1].
    pnorm(x)
}


# =============================================================================
# Distributions of interest
# =============================================================================

kanen2019_beta_plus_normal_clipped <- function(n) {
    group_mean <- rbeta(
        n = n,
        shape1 = PRIOR_BETADIST_SHAPE1,
        shape2 = PRIOR_BETADIST_SHAPE2
    )
    intersubject_sd <- halfnormal(
        n = n,
        mean = 0,
        sd = PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1
    )
    subject_effect <- rnorm(n = n, mean = 0, sd = intersubject_sd)
    return(clip(group_mean + subject_effect, lower = 0, upper = 1))

    # We have previously implemented the range check this using sampling
    # ("_lp") functions that check boundaries and use "target +=
    # negative_infinity()" if out of range. The equivalent in standard Stan
    # code is the "reject()" function. Stan advises against using reject() for
    # constraints:
    # https://mc-stan.org/docs/reference-manual/reject-statements.html.
    #
    # The safe ways to do constrained sampling:
    #
    # - Explicitly truncated sampling, as per the Stan manual, e.g.
    #
    #       real<lower=a, upper=b> theta;
    #       theta ~ normal(mu, sigma) T[a, b];
    #
    # - Explicit correction, as my commonfunc.stan code supports, e.g.
    #
    #       sampleNormalRangeBound_RRR_lp(theta, mu, sigma, a, b);
    #
    #   ... which (a) corrects "target" for the constraint (applicable if it's
    #   within range), and (b) rejects (by adding −∞ to target) if out of
    #   range.
    #
    # - Sample in an infinite space and transform, e.g.
    #
    #       raw_theta ~ normal(0, 1);  // unconstrained
    #       theta = Phi_approx(raw_theta);  // constrained (0, 1)
    #
    # HOWEVER, my code is likely imperfect at the edge cases where e.g. a group
    # mean is close to the boundary, and the addition of a subject-specific
    # effect knocks it over. I have used my Stan function bound() for this, or
    # clip() here.
}

beta_plus_normal_constrained <- function(n) {
    # As above, but restrict() rather than clip().
    group_mean <- rbeta(
        n = n,
        shape1 = PRIOR_BETADIST_SHAPE1,
        shape2 = PRIOR_BETADIST_SHAPE2
    )
    intersubject_sd <- halfnormal(
        n = n,
        mean = 0,
        sd = PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1
    )
    subject_effect <- rnorm(n = n, mean = 0, sd = intersubject_sd)
    return(restrict(group_mean + subject_effect, lower = 0, upper = 1))
}

ahn2017_constrained <- function(n, lower = 0, upper = 1) {
    # Ahn (2017, PMID 29601060, hBayesDM, pp. 38-40): "For parameters that are
    # bounded between 0 and 1... we use the inverse probit transformation (the
    # cumulative distribution function of a unit normal distribution)... this
    # transformation guarantees that the converted prior will be uniformly
    # distributed between 0 and 1..."
    #
    #       mu_raw_param ~ Normal(0, 1)
    #       sigma_raw_param ~ half-Cauchy(0, 5)  # <-- N.B.
    #       raw_param ~ Normal(mu_raw_param, sigma_raw_param)
    #       param = inverse_probit(raw_param)
    #
    # [And then a simple transformation for other boundaries.]
    #
    # HOWEVER,
    #
    # (1) Empirically, this method was rubbish at converging in lots of studies
    #     in which I've attempted it.
    # (2) And the use of half-Cauchy(0, 5) massively distorts the prior,
    #     creating peaks near 0 and 1 and a central trough. That would explain
    #     the poor convergence! (Discovered 2022-12-14.)
    #
    # To check evolution of their thinking:
    #
    # - Haines (2018, PMID 30289167, Iowa Gambling Task, p. 13), regarding a
    #   reward parameter constrained [0, 1]. Relabelling their variables:
    #
    #       mu_raw_param ~ Normal(0, 1)
    #       sigma_intersubject ~ Normal(0, 0.2)
    #       raw_subject_effect ~ Normal(0, 1)
    #       param = Probit(mu_raw_param + sigma_intersubject * raw_subject_effect)
    #
    #   Note:
    #
    #   - Presumably sigma_intersubject is a positive half-normal.
    #
    #   - sigma_intersubject * raw_subject_effect is N(0, sigma_intersubject),
    #     which is N(0, σ ~ N(0, 0.2)).
    #
    #   - They say that "Probit(x) is the inverse cumulative cumulative
    #     distribution function of the standard normal distribution", which
    #     must be true from Ahn's (2017) definition of inverse probit as the
    #     CDF. However, the **probit** function is the quantile function (the
    #     inverse of the cumulative distribution function) for the standard
    #     normal distribution (https://en.wikipedia.org/wiki/Probit), and thus
    #     maps [0, 1] to [−∞, +∞]. In R, this is ``qnorm()``, as in ``q <-
    #     qnorm(p)``. So it is highly likely that they have defined Probit()
    #     correctly but meant to use inverse probit.
    #
    # - Romeu (2020, PMID 31735532, Cambridge Gambling Task, Supplementary p.
    #   14): "We used a standardized convention for parameterizing all tested
    #   models, which is described in great detail in our previous work (Ahn et
    #   al., 2017; Haines et al., 2018)... We assumed that each
    #   individual-level distribution was drawn from a normally-distributed
    #   group distribution where prior distributions for the means and standard
    #   deviations of each group-level parameter followed normal(0,1) and
    #   half-normal(0,0.2) distributions, respectively."
    #
    # - Github code for hBayesDM, 2022-12-14: e.g. probabilistic reversal task,
    #   https://github.com/CCS-Lab/hBayesDM/blob/cf9a08f227cde90545943ba0e5be23eae3bd8d13/commons/stan_files/prl_rp.stan,
    #   with my comments:
    #
    #       mu_pr ~ normal(0, 1);       // group mean
    #       sigma ~ normal(0, 0.2);     // intersubject SD
    #       Arew_pr ~ normal(0, 1);     // raw N(0, 1) subject deviation
    #   ...
    #       Arew[i]  = Phi_approx(mu_pr[2] + sigma[2] * Arew_pr[i]);
    #
    # So they have shifted from the half-Cauchy(0, 5) to the half-normal(0,
    # 0.2) for SDs.
    #
    # See also my ``stan_notes.rst`` for discussion of half-normal versus
    # half-Cauchy for SD new_quantile_functions.

    group_mean <- std_normal(n)
    # intersubject_sd <- 1e-5  # for exploring removing this effect!
    intersubject_sd <- halfcauchy(n, scale = AHN2017_SD_CAUCHY_SCALE)
    raw_param <- rnorm(n, mean = group_mean, sd = intersubject_sd)
    range <- upper - lower
    return(phi(raw_param) * range + lower)
    # Ultimately, for range 0-1:   Φ(N(μ ~ N(0, 1), σ ~ halfCauchy(0, 5)))
    #                            = Φ(N(0, 1) + N(0, σ ~ halfCauchy(0, 5)))
}

romeu2020_constrained <- function(n, lower = 0, upper = 1) {
    # See above
    group_mean <- std_normal(n)
    intersubject_sd <- halfnormal(n, sd = ROMEU2020_SD_HALFNORMAL_SD)
    raw_subject_effect <- std_normal(n)
    raw_param <- group_mean + intersubject_sd * raw_subject_effect
    range <- upper - lower
    return(phi(raw_param) * range + lower)
    # Ultimately, for range 0-1: Φ(N(0, 1) + N(0, σ ~ halfNormal(0, 0.2)))

    # It's better than Ahn 2017, but it's still slightly bimodal.
}

normal_uniform_arbitrary_beta <- function(n) {
    # A different approach: work with raw parameters in the N(0, 1) space but
    # use a different transformation, via Φ() to give a uniform distribution,
    # through qbeta() to give (in this example) a beta distribution. Or any
    # other (slightly modified) prior, for that matter.
    group_mean <- std_normal(n)
    # ... N(0, 1)
    intersubject_sd <- halfnormal(n, sd = ROMEU2020_SD_HALFNORMAL_SD)
    raw_subject_effect <- std_normal(n)
    raw_param <- group_mean + intersubject_sd * raw_subject_effect
    # ... N(0, 1) + N(0, intersubject_sd)
    p <- phi(raw_param)
    return(
        qbeta(
            p,
            shape1 = PRIOR_BETADIST_SHAPE1,
            shape2 = PRIOR_BETADIST_SHAPE2
        )
    )

    NOTES <- "
        // The equivalent in 'plain' Stan would be:

        data {
            int<lower=1> N_GROUPS;
            int<lower=1> N_SUBJECTS;
            int<lower=1, upper=N_GROUPS> group[N_SUBJECTS];
        }
        parameters {
            real raw_group_mean[N_GROUPS];
            real<lower=0> raw_intersubject_sd;  // assume common variance
            real raw_subject_effect_std_normal[N_SUBJECTS];

        }
        transformed parameters {
            real parameter[N_SUBJECTS];
            for (s in 1:N_SUBJECTS) {
                parameter[s] = ***qbeta_equivalent***(
                    Phi_approx(
                        raw_group_mean[group[s]]
                        + raw_intersubject_sd * raw_subject_effect[s]
                    )
                );
            }
        }
        model {
            raw_group_mean ~ normal(0, 1);
            raw_intersubject_sd ~ normal(0, ROMEU2020_SD_HALFNORMAL_SD);
            raw_subject_effect_std_normal ~ normal(0, 1);
        }
    "
}


# =============================================================================
# Demonstration data
# =============================================================================

PARAMNAME_01 <- "param [0, 1]"
PARAMNAME_0_INF <- "param [0, +∞)"
PARAMNAME_UNCONSTRAINED <- "param (−∞, +∞)"
PARAMNAME_SD <- "param SD [0, +∞)"
PARAMNAME_EXPANDED <- "param with intersubject variability (−∞, +∞)"

priors_01 <- rbind(
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Beta(", PRIOR_BETADIST_SHAPE1, ", ", PRIOR_BETADIST_SHAPE2, ")"
        ),
        x = rbeta(
            n = N_VALUES,
            shape1 = PRIOR_BETADIST_SHAPE1,
            shape2 = PRIOR_BETADIST_SHAPE2
        )
    ),
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Kanen2019: Beta(",
            PRIOR_BETADIST_SHAPE1,
            ", ",
            PRIOR_BETADIST_SHAPE2,
            ") + N(0, σ ~ N<sup>+</sup>(0, ",
            PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1,
            ")), clipped [0,1]"
        ),
        x = kanen2019_beta_plus_normal_clipped(N_VALUES)
    ),
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Beta(",
            PRIOR_BETADIST_SHAPE1,
            ", ",
            PRIOR_BETADIST_SHAPE2,
            ") + N(0, σ ~ N<sup>+</sup>(0, ",
            PRIOR_HALF_NORMAL_SD_FOR_SD_IN_RANGE_0_1,
            ")), if ∈ [0,1]"
        ),
        x = beta_plus_normal_constrained(N_VALUES)
    ) %>% filter(!is.na(x)),
    data.frame(
        parameter = PARAMNAME_01,
        method = "Φ(N(0, 1)), in theory flat",
        x = phi(std_normal(N_VALUES))
    ),
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Ahn2017: Φ(N(0, 1) + N(0, σ ~ Cauchy<sup>+</sup>(0, ",
            AHN2017_SD_CAUCHY_SCALE,
            ")))"
        ),
        x = ahn2017_constrained(N_VALUES)
    ),
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Romeu2020: Φ(N(0, 1) + N(0, σ ~ N<sup>+</sup>(0, ",
            ROMEU2020_SD_HALFNORMAL_SD,
            ")))"
        ),
        x = romeu2020_constrained(N_VALUES)
    ),
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Beta(", PRIOR_BETADIST_SHAPE1, ", ", PRIOR_BETADIST_SHAPE2,
             ") as Q<sub>Beta</sub>(Φ[N(0, 1)], ",
             PRIOR_BETADIST_SHAPE1, ", ", PRIOR_BETADIST_SHAPE2, ")"
        ),
        x = qbeta(
            pnorm(rnorm(N_VALUES)),
            shape1 = PRIOR_BETADIST_SHAPE1,
            shape2 = PRIOR_BETADIST_SHAPE2
        )
    ),
    data.frame(
        parameter = PARAMNAME_01,
        method = paste0(
            "Q<sub>Beta</sub>(Φ[N(0, 1) + N(0, σ ~ N<sup>+</sup>(0, ",
            ROMEU2020_SD_HALFNORMAL_SD, "))], ",
            PRIOR_BETADIST_SHAPE1, ", ", PRIOR_BETADIST_SHAPE2, ")"
        ),
        x = normal_uniform_arbitrary_beta(N_VALUES)
        # This is not the same as Beta(1.2, 1.2), but it is very close (and
        # just dispersed a bit towards the edges).
    )
) %>% as_tibble()

priors_0_inf <- rbind(
    data.frame(
        parameter = PARAMNAME_0_INF,
        method = "N<sup>+</sup>(0, 1)",
        x = halfnormal(N_VALUES)
    ),
    data.frame(
        parameter = PARAMNAME_0_INF,
        method = "exp{ N(0, 1) }",
        x = exp(std_normal(N_VALUES))
    ),
    data.frame(
        parameter = PARAMNAME_0_INF,
        method = "Gamma(4.82, 0.88)",
        x = rgamma(N_VALUES, shape = 4.82, rate = 0.88)
    )
) %>% as_tibble()

priors_unconstrained <- rbind(
    data.frame(
        parameter = PARAMNAME_UNCONSTRAINED,
        method = "N<sup>+</sup>(0, 1)—just to check it",
        x = halfnormal(N_VALUES)
    ),
    data.frame(
        parameter = PARAMNAME_UNCONSTRAINED,
        method = "N(0, 1)",
        x = std_normal(N_VALUES)
    ),
    data.frame(
        parameter = PARAMNAME_UNCONSTRAINED,
        method = "N(1.5, 1)",
        x = rnorm(N_VALUES, mean = 1.5, sd = 1)
    ),
    data.frame(
        parameter = PARAMNAME_UNCONSTRAINED,
        method = "N(0, σ = 3)",
        x = rnorm(N_VALUES, mean = 0, sd = 3)
    ),
    data.frame(
        parameter = PARAMNAME_UNCONSTRAINED,
        method = "N(0, σ = 5)",
        x = rnorm(N_VALUES, mean = 0, sd = 5)
    ),
    data.frame(
        parameter = PARAMNAME_UNCONSTRAINED,
        method = "Cauchy(0, 5)",
        x = rcauchy(N_VALUES, location = 0, scale = 5)
    )
) %>% as_tibble()

priors_sd <- rbind(
    data.frame(
        parameter = PARAMNAME_SD,
        method = "N+(0, σ = 0.05)",
        x = halfnormal(N_VALUES, mean = 0, sd = 0.05)
    ),
    data.frame(
        parameter = PARAMNAME_SD,
        method = "N+(0, σ = 0.1)",
        x = halfnormal(N_VALUES, mean = 0, sd = 0.1)
    ),
    data.frame(
        parameter = PARAMNAME_SD,
        method = "N+(0, σ = 0.5)",
        x = halfnormal(N_VALUES, mean = 0, sd = 0.5)
    )
) %>% as_tibble()

# Var(X + Y) = Var(X) + Var(Y) + 2 * Cov(X, Y)
# The variance of a normal RV is the square of its SD. So:
intersubject_variability <- rbind(
    data.frame(
        parameter = PARAMNAME_EXPANDED,
        method = "N(0, 1)",
        x = rnorm(N_VALUES, mean = 0, sd = 1)
    ),
    data.frame(
        parameter = PARAMNAME_EXPANDED,
        method = "N(0, σ<sup>2</sup> = 1<sup>2</sup> + 0.05<sup>2</sup>)",
        x = rnorm(N_VALUES, mean = 0, sd = sqrt(1 + 0.05^2))
    ),
    data.frame(
        parameter = PARAMNAME_EXPANDED,
        method = "N(0, σ<sup>2</sup> = 1<sup>2</sup> + 0.5<sup>2</sup>)",
        x = rnorm(N_VALUES, mean = 0, sd = sqrt(1 + 0.5^2))
    ),
    data.frame(
        parameter = PARAMNAME_EXPANDED,
        method = "N(0, σ<sup>2</sup> = 1<sup>2</sup> + 1<sup>2</sup>)",
        x = rnorm(N_VALUES, mean = 0, sd = sqrt(1 + 1))
    )
) %>% as_tibble()

COMMON_GRAPH_ELEMENTS <- list(
    facet_grid(. ~ parameter),
    theme_bw(),
    theme(legend.text = element_markdown())
)
p_01 <- (
    ggplot(priors_01, aes(x = x, colour = method))
    + geom_density(size = LINEWIDTH, n = KERNEL_DENSITY_N)
    + COMMON_GRAPH_ELEMENTS
)
p_0_inf <- (
    ggplot(priors_0_inf, aes(x = x, colour = method))
    + geom_density(size = LINEWIDTH, n = KERNEL_DENSITY_N)
    + xlim(-1, 15)
    + COMMON_GRAPH_ELEMENTS
)
p_unconstrained <- (
    ggplot(priors_unconstrained, aes(x = x, colour = method))
    + geom_density(size = LINEWIDTH, n = KERNEL_DENSITY_N)
    + xlim(-8, 8)
    + COMMON_GRAPH_ELEMENTS
)
p_sd <- (
    ggplot(priors_sd, aes(x = x, colour = method))
    + geom_density(size = LINEWIDTH, n = KERNEL_DENSITY_N)
    + COMMON_GRAPH_ELEMENTS
)
p_expanded <- (
    ggplot(intersubject_variability, aes(x = x, colour = method))
    + geom_density(size = LINEWIDTH, n = KERNEL_DENSITY_N)
    + COMMON_GRAPH_ELEMENTS
)
p <- (p_01 / p_0_inf / p_unconstrained / p_sd / p_expanded)
ggsave(OUTPUT_PNG, p, units = "cm", width = 25, height = 60)
# ... PDF doesn't get the theta character right
