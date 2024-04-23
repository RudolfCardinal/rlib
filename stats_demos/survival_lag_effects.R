#!/usr/bin/env Rscript
#
# survival_lag_effects.R
#
# Effects of correctly or incorrectly left-censoring survival data.
# RNC, 26 Nov 2019.

# =============================================================================
# Reminders
# =============================================================================
# Left censoring: data point is below a value (but we don't know how much).
# ... we know the UPPER BOUND
#
# Right censoring: data point is above a value (but we don't know how much).
# ... we know the LOWER BOUND
# ... e.g. "still alive at end of study, event hasn't happened"
# ... we have a LOWER BOUND on their time period
#
# Interval censoring: data point between two values
# ... we know LOWER AND UPPER BOUNDS
#
# Note easy confusion between left/right censoring:
# - https://stats.stackexchange.com/questions/144037/right-censoring-and-left-censoring

# =============================================================================
# Libraries, seed
# =============================================================================

library(data.table)
library(survival)

# For interval-censored (left- and right-censored) data:
# library(coxinterval)  # for coxaalen() -- BUT "Required CPLEX-dependent libraries are unavailable."
# library(MIICD)  # for MIICD.coxph(), but very slow
# library(timereg)  # for aalen()
# library(icenReg)  # see https://cran.r-project.org/web/packages/icenReg/vignettes/icenReg.pdf

set.seed(1234)
SEP_EQ = "===============================================================================\n"

# =============================================================================
# Things of interest
# =============================================================================

RELATIVE_CONDITION_HAZARD_ACTIVE <- 1  # 1 is "no difference"
START_LAG_BIAS <- 30  # active group start this much later
# ... with a naive model, a positive lag makes the active group look less prone
#     to the event (or the control group more)

# =============================================================================
# Create data
# =============================================================================
CONDITION_HAZARD_CONTROL <- 0.01
CONDITION_HAZARD_ACTIVE <- CONDITION_HAZARD_CONTROL * RELATIVE_CONDITION_HAZARD_ACTIVE
DEATH_HAZARD_CONTROL <- 0.005
DEATH_HAZARD_ACTIVE <- DEATH_HAZARD_CONTROL

N_CONTROL <- 50
N_ACTIVE <- 50
STUDY_START_TIME <- 0
STUDY_END_TIME <- 100
CONTROL <- "control"
ACTIVE <- "active"

d <- data.table(subject = 1:(N_ACTIVE + N_CONTROL))
d[, group := factor(ifelse(subject <= N_CONTROL, CONTROL, ACTIVE),
                    levels = c(CONTROL, ACTIVE))]

d[, entry_time := STUDY_START_TIME + ifelse(group == ACTIVE, START_LAG_BIAS, 0)]

# Simulating survival:
# - https://stats.stackexchange.com/questions/135124/how-to-create-a-toy-survival-time-to-event-data-with-right-censoring
# - https://cran.r-project.org/web/packages/coxed/vignettes/simulating_survival_data.html

d[group == CONTROL, time_entry_to_death := rexp(n = N_CONTROL, rate = DEATH_HAZARD_CONTROL)]
d[group == ACTIVE, time_entry_to_death := rexp(n = N_ACTIVE, rate = DEATH_HAZARD_ACTIVE)]
d[group == CONTROL, time_entry_to_condition := rexp(n = N_CONTROL, rate = CONDITION_HAZARD_CONTROL)]
d[group == ACTIVE, time_entry_to_condition := rexp(n = N_ACTIVE, rate = CONDITION_HAZARD_ACTIVE)]

d[, death_time := entry_time + time_entry_to_death]
d[, condition_time := entry_time + time_entry_to_condition]

d[, died := death_time < STUDY_END_TIME]
d[, condition := condition_time < death_time & condition_time < STUDY_END_TIME]
d[, end_time := pmin(condition_time, death_time, STUDY_END_TIME)]

# =============================================================================
# Model 1 is ignorant of start times (left censoring) and is biased
# =============================================================================

cat(paste0(SEP_EQ, "Wrong model\n", SEP_EQ))
s1 <- survival::Surv(time = d$end_time, event = d$condition, type = "right")
# When you print it:
# - times are "time to event or censoring"
#   - in our toy example, "event" is "developed condition" and "censoring" is
#     "died or reached the end of the study without developing the condition"
# - "+" indicates censoring

m1 <- survival::coxph(s1 ~ group, data = d)
print(m1)

# OBSERVE:
# - When there is no difference between groups
#   (RELATIVE_CONDITION_HAZARD_ACTIVE == 1), a spurious difference is generated
#   via the wrong analysis s1/m1 if START_LAG_BIAS is non-zero.

# =============================================================================
# Model 2 is better.
# It doesn't use interval censoring, but it corrects for start time.
# =============================================================================

cat(paste0(SEP_EQ, "Correct model\n", SEP_EQ))
s2 <- survival::Surv(time = d$end_time - d$entry_time, event = d$condition, type = "right")
m2 <- survival::coxph(s2 ~ group, data = d)
print(m2)

# =============================================================================
# What follows was an attempt at interval censoring -- but that's wrong.
# Interval censoring is for when the "event time" is uncertain. See
# https://cran.r-project.org/web/packages/icenReg/vignettes/icenReg.pdf
# =============================================================================

IGNORE <- '
cat(paste0(SEP_EQ, "Trying an interval-censoring model\n", SEP_EQ))
d[, left := entry_time]
d[, right := end_time]

s3_a <- survival::Surv(time = d$left, time2 = d$right,
                       event = as.numeric(d$condition),
                       type = "interval")

s3_b <- survival::Surv(time = d$left, time2 = d$right,
                       type = "interval2")
'

# Note:
# - timereg::aalen() uses Surv(<left>, <right>, type = "interval2")
#   where the interval is (<left>, <right>].
# - its convention is that infinite values can -Inf, +Inf, or NA; NA is
#   generally preferred.

# -----------------------------------------------------------------------------
# survival::coxph() ?
# -----------------------------------------------------------------------------

FAILS <- '
m3 <- survival::coxph(s3 ~ group, data = d)
'
# ... nope: 'Cox model doesn't support "interval" survival data'

# -----------------------------------------------------------------------------
# MIICD::MIICD.coxph() ?
# -----------------------------------------------------------------------------

TOO_SLOW <- '
N_ITERATIONS <- 5  # as per ?MIICD.coxph
N_IMPUTATIONS_PER_ITERATION <- 5  # as per ?MIICD.coxph
m3 <- MIICD::MIICD.coxph(formula = ~ group, data = d,
                         k = N_ITERATIONS, m = N_IMPUTATIONS_PER_ITERATION)
'
# ... *extremely* slow and this is a toy example

# -----------------------------------------------------------------------------
# timereg::aalen() ?
# -----------------------------------------------------------------------------

IGNORE <- '
m3 <- timereg::aalen(s3_b ~ group, data = d)
print(m3)
'
