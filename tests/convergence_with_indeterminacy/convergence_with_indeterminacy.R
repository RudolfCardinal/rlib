#!/usr/bin/env Rscript

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    rstan
)

set.seed(1234)


STANCODE <- '

transformed data {
    array[5] real y = {4.8, 4.9, 5.0, 5.1, 5.2};  // mean 5, SD 0.158
}
parameters {
    real mu_good;
    real mu_bad_1;
    real mu_bad_2;
    real<lower=0> sigma;
}
model {
    mu_good ~ normal(0, 10);
    mu_bad_1 ~ normal(0, 10);
    mu_bad_2 ~ normal(0, 10);
    sigma ~ normal(0, 10);

    y ~ normal(mu_good, sigma);
    y ~ normal(mu_bad_1 + mu_bad_2, sigma);
}

'

fit <- rstan::stan(model_code = STANCODE)
print(fit)

NOTES <- '

- mu_bad_1 and mu_bad_2 have very wide confidence intervals, but the chains do
  converge.


'

# NOT YET A USEFUL TEST. But see lectures/2021/12.../demonstrate_poor_stan_convergence.R
