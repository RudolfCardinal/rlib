#!/usr/bin/Rscript
#
# testing_nested_models.R
#
# A very basic demo.

library(data.table)
n <- 60
coeff_a <- 3
coeff_b <- 1

d <- data.table(
    a = rnorm(n, mean = 20, sd = 5),
    b = rnorm(n, mean = 5, sd = 5),
    c = rnorm(n, mean = 25, sd = 5),
    err = rnorm(n, mean = 0, sd = 1)
)
d[, y := coeff_a * a + coeff_b * b + err]

cat("Creating models m1 < m2 < m3, all for the same data, d.\n")
cat("Predictors in m1 and m2 are relevant, but the additional predictor in m3 is not.\n")

m1 <- lm(y ~ a, data = d)
m2 <- lm(y ~ a + b, data = d)
m3 <- lm(y ~ a + b + c, data = d)

cat("\n--- anova(m1, m2):\n")
print(anova(m1, m2))  # smaller, larger (where smaller is a subset model of larger)
cat("\n--- anova(m2, m3):\n")
print(anova(m2, m3))
