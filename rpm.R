# rpm.R

# http://en.wikipedia.org/wiki/Bayesian_inference -- good on the archaeology example
# http://en.wikipedia.org/wiki/Checking_whether_a_coin_is_fair
# We wish to estimate the probability density function that a particular outcome delivers reward.
# Initially, the PDF is flat from 0-1.
# Encoding a PDF: well... could take infinite information, in principle...
# So we may as well encode it mathematically, and assume the Bayesian model is a perfect one.
# Therefore we have
# - p = probability of reward
# - N = number of trials so far
# - x = number of rewards on those trials
# - y = number of punishments on those trials
# Then, for a uniform prior, the posterior is
# - f(p | x rewards and y punishments in N trials) = ( (N+1)! / x! y! ) * p^x * (1 - p)^y ... or a beta-distribution alternative that won't break computationally.

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

rpm = new.env()

#-------------------------------------------------------------------------------
# Posteriors for binomial things
#-------------------------------------------------------------------------------

rpm$posterior_pdf_given_uniform_prior_with_two_outcomes <- function(p, N, x)
{
    y = N - x
    (1 / beta(x + 1, y + 1) ) * p^x * (1 - p)^y
}

rpm$best_estimator_of_p_given_uniform_prior_with_two_outcomes <- function(N, x)
{
    x/N # obviously
    # maximum a posteriori (MAP) estimate
    # ... a mode of the posterior distribution
}

#-------------------------------------------------------------------------------
# Illustration
#-------------------------------------------------------------------------------

rpm$demo.posterior.binomial <- function()
{
    p = seq(0,1,0.01)
    plot(p, posterior_pdf_given_uniform_prior_with_two_outcomes(p, 5, 1))
}

# NEXT: model in which
# (a) subjects run the best delta-rule model (td5_*)
# (b) subjects track N (number of trials on which a particular option chosen) and a (... number of those trials on which reward was delivered [as opposed to punishment])
# (c) subjects have a fixed weight between the two models (0 completely delta-rule, 1 completely based on whole history)
# ... NO - still wouldn't differentiate between "true" and "false" punishment...
#     HANG ON, IT MIGHT - if most "true" punishment occurs early on, then the subject switches to the "good" option and becomes more fixed in its beliefs (and then most punishment is "false")
#
# CHOICE RULE given two pdfs... want to yield a probability of picking A given PDFs for A and B
# - Gittins (1972) index? No.
# - randomized probability matching (RPM) (Scott 2010)

#-------------------------------------------------------------------------------
# RPM APPLIED TO THE BINOMIAL BANDIT
#-------------------------------------------------------------------------------

# In what follows:
#   y = cumulative number of successes for each action = vector of length N_ACTIONS containing number of wins for each action
#   n = cumulative number of trials for each action = vector of length N_ACTIONS containing number of trials for each action (thus, number of losses = n - y)

rpm$compute.probopt <- function(y, n)
{
    k <- length(y) # k is the number of actions
    ans <- numeric(k) # zero vector of length k
    for (i in 1:k) { # for each action:
        indx <- (1:k)[-i] # list of all actions except the current one
        f <- function(x) { # x stands for theta_a in equation 11
            r <- dbeta(x, y[i] + 1, n[i] - y[i] + 1) # ... beta density function (dbeta), for the current action; dbeta(x,a,b) is the density at x of the beta distribution for a random variable with parameters a and b
            for (j in indx) { # for all the other actions
                r <- r * pbeta(x, y[j] + 1, n[j] - y[j] + 1) # ... for the other actions; pbeta(x,a,b) is the probability that the RV with parameters a,b is less than x.
            }
            return(r)
        }
        ans[i] = integrate(f, 0, 1)$value # w is the integral from 0 to 1 of the function defined (equation 11 in Scott 2010)
    }
    return(ans)
}

rpm$sim.post <- function(y, n, ndraws)
{
    k <- length(y)
    ans <- matrix(nrow = ndraws, ncol = k)
    no <- n - y
    for (i in 1:k) {
        ans[,i] <- rbeta(ndraws, y[i] + 1, no[i] + 1) # rbeta draws an RV with parameters a,b
    }
    return(ans)
}

rpm$prob.winner <- function(post)
{
    k <- length(y) # RNC: was ncol(y) in the original but that fails for vectors
    w <- table(factor(max.col(post), levels = 1:k))
    return( w / sum(w) )
}

rpm$compute.win.prob <- function(y, n, ndraws)
{
    return( prob.winner( sim.post(y, n, ndraws) ) )
}

#-------------------------------------------------------------------------------
# Illustration
#-------------------------------------------------------------------------------

rpm$demo.rpm <- function()
{
    y = c(20 - 1, 20 - 1)
    no = c(30 - 1, 10 - 1)
    n = y + no
    
    compute.probopt(y, n)
    compute.win.prob(y, n, 1000)
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("rpm" %in% search()) detach("rpm")
attach(rpm)  # subsequent additions not found, so attach at the end
