Survival analysis: Cox proportional hazards versus logistic regression
======================================================================

One might guess that they are the same, i.e. "time" is defined from a subject's
start to end (end being the "event" or right-censorship); outcome is then
binary; and we might have

    logit(outcome) ~ intercept + (predictors...) * time

where

    logit = log(p) - log(1 - p) = log(p / (1 - p)) = log(odds)

which would embody the "proportional hazards" (time-invarying) assumption of
the Cox method.

But is that true?


Theory
------

- Rate models [Rothman2008, p. 395].
  Notation:
  - I(x) = average rate for group  (p396).
  - Rates must be constrained to be zero/positive (p396).
  - That's commonly done as I(x) = exp(...), the exponential rate model.
  - Which is equivalent to the log-linear rate model ln(I(x)) = [...].

- "Incidence-time" = "survival-time" models [Rothman2008, p. 396].

- A common approach to modelling incidence times define the risk "at" a time
  (per unit time), or the risk "up to" a given time. The commonest is the
  "proportional hazards" model = Cox model [Rothman2008, p. 397]:

    I(t; x) ≈ exp(α_t + β⋅x)

  where β is assumed to be constant. (Though α_t may vary with time.)

  If x (here taken to be a variable measuring exposure to something of
  interest) is 0, then I(t; 0) ≈ exp(α_t), and exp(α_t) can be denoted h0(t),
  the "baseline" rate. Cox models are often expressed like this (right-most
  form):

    I(t; x) ≈ exp(α_t + βx) = exp(α_t)⋅exp(βx) = exp(β⋅x)⋅h0(t)

  Incidences are defined over a small time period Δt, and as Δt → 0, I(t; x)
  approaches a limit h(t; x), "usually called the hazard or intensity of the
  outcome at time t. The Cox model is then defined as a model for these
  hazards,"

    h(t; x) = exp(β⋅x)⋅h0(t)

- THIS IS ALSO EXPRESSED WELL HERE:

  - https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_survival/BS704_Survival6.html
  - https://stats.stackexchange.com/questions/4528/what-is-the-difference-between-the-coef-and-expcoef-output-of-coxph-in-r

- "The Cox model with time-dependent covariates.. is..."

    I(t; x[t]) ≈ h(t; x[t]) = exp(α_t)⋅exp(βx[t]) = exp(β⋅x)⋅h0(t)

  "This model may be the most widely used... for time-dependent exposures."
  [Rothman2008, p397.]


Common terminology
------------------

It seems common to speak of logistic regression models as being
time-independent (i.e. did an event happen or not, regardless of when?), versus
Cox proportional hazards models, which take account of the time until events
occur, in which case the latter are more powerful (e.g. [vanderNet2008]). Fair
enough. But I meant logistic regression using time as a predictor also.

A good article seems [Ngwa2016]. They note

    POOLED LOGISTIC REGRESSION (PLR)

    "The use of standard logistic regression techniques to estimate hazard
    rates was detailed by Efron [Efron1988]. His approach, known as partial
    logistic regression, entailed the use of parametric logistic regression
    modeling on censored data to obtain estimates and standard errors. The
    pooled repeated observations approach, described by Cupples et al. [5], has
    been frequently employed in the Framingham Heart Study. In this method each
    observation interval is considered a mini-follow up study in which the
    current risk factors are updated to predict events in the interval. Once an
    individual has an event in a particular interval all subsequent intervals
    from that individual are excluded from the analysis."

    CROSS SECTIONAL POOLING (CSP)

    "The CSP uses Cox regression within interval to utilize information on the
    length of time to event within each interval as well as whether or not the
    event occurs..."

    CONCLUSIONS

    "... Thus, when time is available, we recommend using the TDCM or
    equivalently the stratified CSP approaches with time intervals."


Implementation
--------------


References
==========

[Efron1988] https://doi.org/10.2307/2288857
[Rothman2008] Rothman et al. (2008) "Modern Epidemiology".
[vanderNet2008] https://doi.org/10.1038/ejhg.2008.59
[Ngwa2016] https://doi.org/10.1186/s12874-016-0248-6
