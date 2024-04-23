#!/usr/bin/Rscript
#
# lmer_confidence_intervals.R
#
# Show confidence intervals on models created using lmer().


library(lme4)
library(lmerTest)


cat("\n--- Create lmer() model using demo data:\n")
# From ?lmer:

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# Note that the summary is more extensive with lmerTest loaded than without:

s1 <- summary(fm1)
print(s1)

# Confidence intervals (works with or without lmerTest loaded):

cat("\n--- Confidence intervals via confint():\n")
print(confint(fm1))

# ... "Computing profile confidence intervals ..."

# MORE EXPLANATION:
# - https://stats.stackexchange.com/questions/117641/how-trustworthy-are-the-confidence-intervals-for-lmer-objects-through-effects-pa


# Via bootstrapping:
# https://rdrr.io/cran/lme4/man/confint.merMod.html

cat("\n--- Confidence intervals via bootstrapping: confint.merMod(..., method = 'boot'):\n")
print(confint.merMod(fm1, method = "boot"))
