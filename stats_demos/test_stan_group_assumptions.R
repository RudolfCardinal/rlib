INFO <- "
RNC, 15 Oct 2024.

An anonymous referee appears to assert that embedding a 'group' variable into a
Stan analysis, in the way we've previously done, creates a bias towards finding
a group difference. I disagree. Our approach is logically equivalent to
analysing two groups in a t test; just testing for a group difference doesn't
create one. (The referee prefers a model comparison approach.)

Firstly, this relates to the points made by Kruschke (2011), PubMed ID (PMID)
26168520, about parameter estimation versus model comparison. Both are fine,
though typically the parameter estimation approach provides more information
(e.g. if there is a difference, how much is it?). In this situation, the model
comparison approach asks: 'is a model with two groups preferable to a model
with one group?' The parameter estimation approach asks: 'what is the best
estimate of the difference between groups (which might be zero)?'

Secondly, this is empirically demonstrable, as follows. We can set up a 'null'
situation in which two pseudo-groups do not in fact differ, and then analyse
using a parameter estimation approach, asking what the group difference (the
difference between group means) is. If the process is unbiased, then since the
true group mean difference is zero, zero should appear within the 95% posterior
highest density interval in approximately 95% of independent tests.

"

library(rstan)

SEED <- 1234  # for the random number generator (pseudorandom replicability)
N_TESTS <- 1000
TRUE_MEAN <- 5.0
TRUE_SD <- 2.0
N_GROUPS <- 2
N_PER_GROUP <- 50

N_SUBJECTS <- N_PER_GROUP * N_GROUPS

STAN_CODE <- "
    data {
        // Subject/group structure:
        int<lower=1> N_GROUPS;
        int<lower=1> N_SUBJECTS;
        int<lower=1, upper=N_GROUPS> group[N_SUBJECTS];

        // Data:
        real y[N_SUBJECTS];
    }
    parameters {
        real group_mean[N_GROUPS];
        real<lower=0> intersubject_sd;  // assume common variance
    }
    transformed parameters {
        // This is the quantity of interest. The true difference is zero,
        // so 0 should fall within the posterior 95% HDI on ~95% of
        // simulations.
        real group_mean_difference = group_mean[2] - group_mean[1];
    }
    model {
        // Priors, which do not in any way differ between groups:
        group_mean ~ normal(0, 5);  // NB >1 value but same prior
        intersubject_sd ~ normal(0, 1);

        // Model
        for (s in 1:N_SUBJECTS) {
            y[s] ~ normal(group_mean[group[s]], intersubject_sd);
        }
    }
"
STAN_MODEL <- rstan::stan_model(
    model_name = "test_group_bias",
    model_code = STAN_CODE
)  # compile once only

run_test <- function(testnum) {
    cat("Running test ", testnum, "...\n", sep = "")
    # Synthesize data. Note that subjects are labelled arbitrarily with group
    # numbers, but there is no difference between the two pseudo-groups.
    standata <- list(
        N_GROUPS = N_GROUPS,
        N_SUBJECTS = N_SUBJECTS,
        group = rep(1:N_GROUPS, each = N_PER_GROUP),
        y = rnorm(n = N_SUBJECTS, mean = TRUE_MEAN, sd = TRUE_SD)
    )
    # Run Stan
    stanfit <- rstan::sampling(STAN_MODEL, data = standata)
    # Extract results
    stansummary <- summary(stanfit)
    parameter <- "group_mean_difference"
    hdi_lower <- stansummary$summary[parameter, "2.5%"]
    hdi_upper <- stansummary$summary[parameter, "97.5%"]
    zero_in_hdi <- (hdi_lower <= 0 && 0 <= hdi_upper)
    return(list(
        stanfit = stanfit,
        summary_df = data.frame(
            testnum = testnum,
            parameter = parameter,
            hdi_lower = hdi_lower,
            hdi_upper = hdi_upper,
            zero_in_hdi = zero_in_hdi
        )
    ))
}

set.seed(SEED)
results <- NULL
for (testnum in 1:N_TESTS) {
    results <- rbind(results, run_test(testnum)$summary_df)
}
cat("Proportion of tests in which zero was in the HDI:\n")
print(sum(results$zero_in_hdi) / nrow(results))
# ANSWER, with this seed: 0.951 (finishing around 15:32 on 15 Oct 2024).
# Took somewhere between 1-2 hours to run (not particularly optimised).

# For any further exploration:
# single_specimen <- run_test(testnum = 0)
