# rpm.R

# http://en.wikipedia.org/wiki/Bayesian_inference
#   -- good on the archaeology example
# http://en.wikipedia.org/wiki/Checking_whether_a_coin_is_fair
# We wish to estimate the probability density function that a particular
# outcome delivers reward.
# Initially, the PDF is flat from 0-1.
# Encoding a PDF: well... could take infinite information, in principle...
# So we may as well encode it mathematically, and assume the Bayesian model is
# a perfect one.
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

#------------------------------------------------------------------------------
# Posteriors for binomial things
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# Illustration
#------------------------------------------------------------------------------

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

#------------------------------------------------------------------------------
# RPM APPLIED TO THE BINOMIAL BANDIT
#------------------------------------------------------------------------------

# In what follows:
#   y = cumulative number of successes for each action
#     = vector of length N_ACTIONS containing number of wins for each action
#   n = cumulative number of trials for each action
#     = vector of length N_ACTIONS containing number of trials for each action
# (thus, number of losses = n - y)

rpm$compute.probopt <- function(y, n)
{
    # As per Scott (2010) p648.
    k <- length(y)  # k is the number of actions
    ans <- numeric(k)  # zero vector of length k
    for (i in 1:k) {  # for each action:
        indx <- (1:k)[-i]  # list of all actions except the current one
        f <- function(x) {  # x stands for theta_a in equation 11
            r <- dbeta(x, y[i] + 1, n[i] - y[i] + 1)
            # ... beta density function (dbeta), for the current action;
            # dbeta(x,a,b) is the density at x of the beta distribution for a
            # random variable with parameters a and b
            for (j in indx) {  # for all the other actions
                r <- r * pbeta(x, y[j] + 1, n[j] - y[j] + 1)
                # ... for the other actions; pbeta(x,a,b) is the probability
                # that the RV with parameters a,b is less than x.
            }
            return(r)
        }
        ans[i] = integrate(f, 0, 1)$value
        # ... the integral from 0 to 1 of the function defined (equation 11
        # in Scott 2010)
    }
    return(ans)
}


rpm$compute.probopt.matrix <- function(
        wins,  # matrix[n_situations, n_actions] of count of "wins"
        totals,  # matrix[n_situations, n_actions] of count of "total trials"
        debug_level = 2,
        debug_every_n_rows = 1000,
        trapezium_rule = FALSE,  # should be FALSE; it's less good
        integration_steps = 1000,  # for trapezium rule only
        quick_calc_last_row = TRUE)
{
    # RNC
    #
    # Rows: different conditions.
    # Columns: different actions.
    #
    # We address a matrix (or array) as:
    #       m[row, col]
    #       m[elementnum], with row number changing faster (so down rows in a
    #           column, then move to the next column and start at the top
    #           again)
    #       m[row, ]: an entire row
    #       m[, col]: an entire column

    if (class(wins) != "matrix") {
        stop("wins is not a matrix")
    }
    if (class(totals) != "matrix") {
        stop("totals is not a matrix")
    }
    if (any(dim(wins) != dim(totals))) {
        stop("Incompatible matrices")
    }
    if (length(dim(wins)) != 2) {
        stop("Need matrix layout, i.e. 2D arrays")
    }
    k <- ncol(wins)  # number of actions
    nr <- nrow(wins)
    losses <- totals - wins
    if (any(losses < 0)) {
        stop("Error: a value in ('totals' - 'wins') is negative; bad data")
    }
    if (any(totals < 0)) {
        stop("Error: a value in 'totals' is negative; bad data")
    }
    if (debug_level >= 1) {
        cat("Calculating RPM for:", k, "actions;", nr, "rows\n")
    }

    # Integration by trapezium rule so we can vectorize

    v.dbeta <- Vectorize(dbeta, vectorize.args=c('x', 'shape1', 'shape2'))
    v.pbeta <- Vectorize(pbeta, vectorize.args=c('q', 'shape1', 'shape2'))

    rpmDifferentialSingle <- function(x, rownum, action, others)
    {
        r <- dbeta(x, wins[rownum, action] + 1, losses[rownum, action] + 1)
        for (other in others) {
            r <- r * pbeta(x, wins[rownum, other] + 1, losses[rownum, other] + 1)
        }
        return(r)
    }

    rpmDifferentialMatrix <- function(x)
    {
        # Calculate a single set of values (we'll integrate over many such)
        answer <- matrix(0, nrow=1, ncol=k)
        for (action in 1:k) {
            r <- v.dbeta(x, wins[, action] + 1, losses[, action] + 1)
            others <- (1:k)[-action]  # all actions except the current one
            for (other in others) {
                r <- r * v.pbeta(x, wins[, other] + 1, losses[, other] + 1)
            }
            answer[, action] <- r
        }
        return(answer)
    }

    optimum_probabilities <- matrix(0, nrow=nr, ncol=k)

    # TRAPEZIUM RULE METHOD (but a bit imprecise)

    if (trapezium_rule) {

        # Constants used in the integration:
        x_start <- 0  # lower limit of integral
        x_end <- 1  # upper limit of integral
        x_span <- x_end - x_start
        # integration_steps: number of trapezoids
        dx <- x_span / integration_steps

        f_x_previous <- rpmDifferentialMatrix(x_start)  # values at x=0
        x <- x_start
        for (i in 1:integration_steps) {
            x <- x + dx
            if (debug_level >= 2) {
                cat("Integrating for RPM at", x, "\n")
            }
            f_x <- rpmDifferentialMatrix(x)
            optimum_probabilities <- optimum_probabilities +
                f_x_previous + f_x  # trapezium area * 2 / dx
            f_x_previous <- f_x
        }
        optimum_probabilities <- optimum_probabilities * dx / 2
            # ... finally arriving at the trapezium rule
            # ... more efficient to do the multiplication/division once only

    } else {

        # ROW-BY-ROW APPROACH; not as slow as you'd fear
        # ... because integrate() doesn't allow you to integrate separately
        #     across multiple values returned by a function
        # ... but it is more accurate (and faster) than the trapezium rule.
        for (action in 1:k) {
            if (action == k && quick_calc_last_row && k > 1) {
                    # Since probabilities must sum to 1 in each row:
                    # (... do NOT rely on that during trapezium rule method!)
                optimum_probabilities[, action] <- 1 - rowSums(optimum_probabilities)
                    # ... we do not do rowSums(optimum_probabilities[, -action])
                    # because if k == 2, that converts a matrix to a vector.
                    # Instead, we rely on the fact that we've pre-populated
                    # optimum_probabilities with zeroes.
            } else {
                others <- (1:k)[-action]  # all actions except the current one
                for (rownum in 1:nr) {
                    if (debug_level >= 2 && rownum %% debug_every_n_rows == 0) {
                        cat("Integrating for RPM: action", action,
                            "at row", rownum, "\n")
                    }
                    optimum_probabilities[rownum, action] <- integrate(
                        rpmDifferentialSingle,
                        lower=0,
                        upper=1,
                        rownum=rownum,
                        action=action,
                        others=others
                    )$value
                }
            }
        }
    }

    return(optimum_probabilities)
}

rpm$test.compute.probopt.matrix <- function()
{
    wins <- matrix(
        c(19, 19,
          5, 0,
          19, 19,
          20, 5,
          0, 0),
        ncol=2, byrow=TRUE)
    losses <- matrix(
        c(29, 9,
          5, 1,
          29, 9,
          8, 0,
          0, 0),
        ncol=2, byrow=TRUE)
    totals <- wins + losses
    optprob <- rpm$compute.probopt.matrix(wins, totals)

    cat("Wins:\n")
    print(wins)
    cat("Losses:\n")
    print(losses)
    cat("Totals:\n")
    print(totals)
    cat("Optimum probabilities:\n")
    print(optprob)
    cat("Each row should sum to 1:\n")
    print(rowSums(optprob))
}


rpm$sim.post <- function(y, n, ndraws)
{
    # As per Scott (2010) p649.
    k <- length(y)  # number of actions
    if (length(n) != k) {
        stop("Mismatched parameter sizes to sim.post()")
    }
    ans <- matrix(nrow = ndraws, ncol = k)
    no <- n - y
    for (i in 1:k) {
        ans[,i] <- rbeta(ndraws, y[i] + 1, no[i] + 1)
        # rbeta draws an RV with parameters a,b
    }
    return(ans)
}

rpm$prob.winner <- function(post)
{
    # As per Scott (2010) p649.
    k <- ncol(post)  # RNC: NB fails for vectors
    w <- table(factor(max.col(post), levels = 1:k))
    return( w / sum(w) )
}

rpm$compute.win.prob <- function(y, n, ndraws, debug = TRUE)
{
    # As per Scott (2010) p649.
    sp <- sim.post(y, n, ndraws)
    if (debug) {
        cat("sim.post (columns = actions, rows = simulations, values = p(win):\n")
        print(sp)
    }
    return(prob.winner(sp))
}

#------------------------------------------------------------------------------
# Illustration
#------------------------------------------------------------------------------

rpm$demo.rpm <- function(debug = FALSE)
{
    # Should give 0.0099, 0.9901:
    y <- c(20 - 1, 20 - 1)
    no <- c(30 - 1, 10 - 1)

    # Should give 0.7308, 0.2692:
    #y <- c(5, 0)
    #no <- c(5, 1)

    # Should give 0.5, 0.5:
    #y <- c(0, 0)
    #no <- c(0, 0)

    n <- y + no
    n_sims <- 10000

    cat("Wins for each action:\n")
    print(y)
    cat("Losses for each action:\n")
    print(no)
    cat("Totals for each action:\n")
    print(n)
    cat("Optimal probability with which to perform each action:\n")
    print(compute.probopt(y, n))
    wp <- compute.win.prob(y, n, n_sims, debug=debug)
    cat("Overall win probability per action in", n_sims, "simulations:\n")
    print(wp)
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("rpm" %in% search()) detach("rpm")
attach(rpm)  # subsequent additions not found, so attach at the end
