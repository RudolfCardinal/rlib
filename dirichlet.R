# dirichlet.R

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

dirichlet <- new.env()


#==============================================================================
# Stephan et al. (2009) Neuroimage: Dirichlet parameter estimation
#==============================================================================

dirichlet$dirichlet_exceedance_by_sampling <- function(alpha, Nsamp = 1e6) {
    # See https://github.com/neurodebian/spm8/blob/octave/spm_dirichlet_exceedance.m
    cat("dirichlet_exceedance, Nsamp =", Nsamp, "...\n")
    NK <- length(alpha)
    # Do things in blocks:
    nblk <- ceiling(Nsamp * NK * 8 / 2^28)
    nsamples_in_block <- rep(floor(Nsamp/nblk), nblk)
    if (nblk > 1) {
        nsamples_in_block[ length(nsamples_in_block) ] <- Nsamp - sum( blk[1 : length(blk) - 1])
    }
    cat("nsamples_in_block:", nsamples_in_block, "\n")
    wincounts <- numeric(NK) # Initialize to zero
    histbreaks <- seq(1, NK + 1) - 0.5
    for (i in 1:nblk) {
        cat("block:", i, ", nsamples:", nsamples_in_block[i], "\n")
        # Sample from univariate gamma distribution, then normalize
        r <- matrix(0, nrow = nsamples_in_block[i], ncol = NK)
        for (k in 1:NK) {
            r[,k] <- rgamma(n = nsamples_in_block[i], shape = alpha[k], rate = 1)
        }
        r <- r / colSums(r)
        # Exceedance probabilities
        winners <- apply(r, MARGIN = 1, FUN = which.max)
        wincounts <- wincounts + hist(winners, breaks = histbreaks, plot = FALSE)$counts
    }
    return(wincounts / Nsamp)
}

dirichlet$dirichlet_exceedance_2_models <- function(alpha) {
    c(
        pbeta(0.5, alpha[2], alpha[1]),
        pbeta(0.5, alpha[1], alpha[2])
    )
}

dirichlet$dirichlet_posterior_alpha <- function(
    ln_p, # matrix, N rows, K columns: one value of ln(P(data|fitted model)) for each
    alpha_0 = NULL, # Dirichlet prior (NULL gives default as below)
    Nsamp = 1e6
) {
    cat("dirichlet_posterior_alpha: ln_p =\n")
    print(ln_p)
    # See:
    # - Stephan et al. (2009)
    # - https://github.com/neurodebian/spm8/blob/octave/spm_BMS.m
    # - https://github.com/neurodebian/spm8/blob/octave/spm_Bcdf.m
    #   ... there's a typo: it says "Inverse CDF" at the top, but is really the CDF.
    K <- ncol(ln_p)
    N <- nrow(ln_p)
    if (is.null(alpha_0)) {
        # Usual Dirichlet prior: rep(1, K) where K is the number of models
        alpha_0 <- rep(1, K)
    }
    CONVERGENCE_THRESHOLD <- 1e-10
    alpha <- alpha_0
    convergence_gap <- 1 + CONVERGENCE_THRESHOLD # anything >CONVERGENCE_THRESHOLD is fine, so the "while" runs at least once
    while (convergence_gap > CONVERGENCE_THRESHOLD) {
        # Vectorize as much as possible:
        d1 <- matrix(digamma(alpha), nrow = N, ncol = K, byrow = TRUE)
        d2 <- digamma( sum(alpha) )
        ln_u <- ln_p + d1 + d2
        ln_u <- sign(ln_u) * pmin(MAX_EXPONENT, abs(ln_u)) # prevent numerical problems for badly scaled posteriors
        u <- exp(ln_u)
        beta <- colSums( u / rowSums(u) ) # rowSums() sums over k; colSums() sums over n
        prev_alpha <- alpha
        alpha <- alpha_0 + beta
        convergence_gap <- norm(alpha - prev_alpha, type = "2") # 2-norm of a matrix, following Stephan et al.
        cat("alpha:", alpha, ", convergence_gap:", convergence_gap, "\n")
    }
    exp_r <- alpha / sum(alpha)
    if (K == 2) {
        xp <- dirichlet$dirichlet_exceedance_2_models(alpha)
    } else {
        xp <- dirichlet$dirichlet_exceedance_by_sampling(alpha, Nsamp)
    }
    return(list(
        alpha = alpha, # Dirichlet posterior
        exp_r = exp_r, # expectation of the posterior p(r|y)
        xp = xp # exceedance probabilities
    ))
}

dirichlet$demo.dirichlet <- function() {
    ll_matrix_1 <- matrix(
        c(
            -500, -200, # subject 1
            -600, -250, # subject 2
            -200, -450  # subject 3
        ),
        nrow = 3,
        ncol = 2,
        byrow = TRUE
    )
    ll_matrix_2 <- matrix(
        c(
            -400, -300, -2, # subject 1
            -600, -250, -25, # subject 2
            -200, -450, -3  # subject 3
        ),
        nrow = 3,
        ncol = 3,
        byrow = TRUE
    )
    dirichlet$dirichlet_posterior_alpha(ll_matrix_1, alpha_0 = NULL)
}

# There's also a sampling verification for 2 models, which I've not implemented

# Matlab check:
# LL = [-500 -200; -600 -250; -200 -450]
# spm_BMS(LL)
# spm_BMS(LL, 1e6, 1, 1, 1)


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("dirichlet" %in% search()) detach("dirichlet")
attach(dirichlet)  # subsequent additions not found, so attach at the end
