# stanfunc.R

# Note re data.table:
# ... trailing [] to prevent "doesn't print first time" bug:
# https://stackoverflow.com/questions/32988099/data-table-objects-not-printed-after-returned-from-function
# https://github.com/Rdatatable/data.table/blob/master/NEWS.md#bug-fixes-5


library(rstan)
library(parallel)
library(coda)
library(matrixStats)
library(reshape)
library(ggplot2)
library(bridgesampling)

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

stanfunc = new.env()

#==============================================================================
# Core functions for e.g. rstan 2.16.2:
#==============================================================================

stanfunc$load_or_run_stan <- function(
        data,
        model_code,
        fit_filename,
        model_name,
        save_code_filename = NULL,
        forcerun = FALSE,
        chains = 8,
        iter = 2000,
        init = "0",  # the default, "random", uses the range -2 to +2
        seed = 1234,  # for consistency across runs
        ...)
{
    # Other potential common parameters:
    #   control = list(
    #       adapt_delta = 0.99
    #           # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    #           # https://www.rdocumentation.org/packages/rstanarm/versions/2.14.1/topics/adapt_delta
    #   )

    if (!forcerun && file.exists(fit_filename)) {
        cat("Loading Stan model fit from RDS file: ",
            fit_filename, "...\n", sep="")
        fit <- readRDS(fit_filename)
        cat("... loaded\n")
    } else {
        n_cores_stan <- options("mc.cores")
        n_cores_available <- parallel::detectCores()
        if (n_cores_stan < n_cores_available) {
            warning(paste(
                "Stan is not set to use all available CPU cores; using ",
                n_cores_stan, " when ", n_cores_available,
                " are available; retry after issuing the command\n",
                "    options(mc.cores = parallel::detectCores())",
                sep=""))
        }
        if (n_cores_stan == 1) {
            warning("Running with a single CPU core; Stan may be slow")
        }

        cat(paste("--- Running Stan, starting at", Sys.time(), "...\n"))

        # Stan now supports parallel operation directly
        fit <- rstan::stan(
            model_name = model_name,
            model_code = model_code,
            data = data,
            chains = chains,
            iter = iter,
            init = init,
            seed = seed,
            ...
        )
        cat(paste("... Finished Stan run at", Sys.time(), "\n"))
        cat("--- Saving Stan model fit to RDS file: ",
            fit_filename, "...\n", sep="")
        saveRDS(fit, file=fit_filename)  # load with readRDS()
        cat("... saved\n")
    }

    if (!is.null(save_code_filename) &&
            (forcerun || !file.exists(save_code_filename))) {
        cat("--- Generating C++ code to save...\n")
        stanc_result <- rstan::stanc(model_code = model_code)
        cpp_code <- stanc_result$cppcode

        cat("--- Saving C++ code to file: ",
            save_code_filename, "...\n", sep="")
        cppfile <- file(save_code_filename)
        writeLines(cpp_code, cppfile)
        close(cppfile)
        cat("... saved\n")
    }

    return(fit)
}


stanfunc$load_or_run_bridge_sampler <- function(
    stanfit,
    filename,
    assume_stanfit_from_this_R_session = FALSE,
    model_code = NULL,
    data = NULL,
    cores = parallel::detectCores(),
    forcerun = FALSE,
    ...)
{
    if (!forcerun && file.exists(filename)) {
        cat("Loading bridge_sampler() fit from RDS file: ",
            filename, "...\n", sep="")
        b <- readRDS(filename)
        cat("... loaded\n")
    } else {
        # POTENTIAL PROBLEM:
        #   Error in .local(object, ...) :
        #   the model object is not created or not valid
        # This is a message from rstan::log_prob(stanfit).
        # https://groups.google.com/forum/#!topic/stan-users/uu1p9oGIMhU
        # The FIX is to specify a new stanfit_model, like this.
        if (assume_stanfit_from_this_R_session) {
            cat("Using existing Stan fit as stanfit_model; will crash if the",
                "Stan model was created within a different R session\n")
            stanfit_model <- stanfit
        } else {
            cat("Creating dummy compiled Stan model...\n")
            if (is.null(model_code)) {
                stop("model_code not specified")
            }
            if (is.null(data)) {
                stop("data not specified")
            }
            stanfit_model <- rstan::stan(
                model_code=model_code,
                data=data,  # if you use data=list(), it segfaults
                chains=1,
                iter=1  # despite the bridgesampling help, iter=0 causes an error
            )
            cat("... done\n")
        }
        cat(paste("--- Running bridge_sampler, starting at",
                  Sys.time(), "...\n"))
        b <- bridgesampling::bridge_sampler(
            samples=stanfit,
            stanfit_model=stanfit_model,
            cores=cores,
            ...
        )

        cat(paste("... Finished bridge_sampler run at", Sys.time(), "\n"))
        cat("--- Saving bridge_sampler() fit to RDS file: ",
            filename, "...\n", sep="")
        saveRDS(b, file=filename)  # load with readRDS()
        cat("... saved\n")
    }
    return(b)
}


stanfunc$compare_model_evidence <- function(bridgesample_list,
                                            priors = NULL,
                                            detail = FALSE)
{
    # bridgesample_list
    #   a list or vector whose names are the names of the models, and whose
    #   values are the results of the bridgesampling::bridge_sampler() function
    #
    # priors:
    #   optional, but can be a vector containing prior probabilities for
    #   each model
    #
    # Note:
    # - "marginal likelihood" is the same as "evidence" (e.g. Kruschke 2011
    #   p57-58)

    # https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
    # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
    d <- data.table(
        t(
            vapply(
                X=seq_along(bridgesample_list),
                FUN=function(y, n, i) {
                    c(i,  # index
                      n[[i]],  # model_name
                      y[[i]]$logml)  # log_marginal_likelihood
                },
                FUN.VALUE=c("index"=0,
                            "model_name"="dummy",
                            "log_marginal_likelihood"=0),
                y=bridgesample_list,
                n=names(bridgesample_list)
            )
        )
    )
    d[, index := as.numeric(index)]
    d[, log_marginal_likelihood := as.numeric(log_marginal_likelihood)]

    d[, model_rank := frank(-log_marginal_likelihood,
                            ties.method="min")]  # "sports method"
    # ... bigger (less negative) is better
    # ... and rank() ranks from smallest (-> 1) to biggest, so want the reverse
    # ... and data.table::frank is quicker than rank (not that we care here!)

    n_models <- nrow(d)
    if (is.null(priors)) {
        # Flat priors
        d[, prior_p_model := 1/n_models]
    } else {
        # User-specified priors
        if (length(priors) != n_models) {
            stop("priors: wrong length")
        }
        if (sum(priors) != 1) {
            warning("priors sum to ", sum(priors), ", not 1")
        }
        d[, prior_p_model := priors]
    }

    # Work with logs or everything will overflow.
    d[, log_prior_p_model := log(prior_p_model)]

    # e.g. Grounau 2017 eq 2:
    #                        marginal_likelihood[i] * prior[i]
    # posterior_p_model[i] = ---------------------------------------------------
    #                        sum_over_all_j( marginal_likelihood[j] * prior[j] )
    #
    # Taking logs:
    #
    # log(posterior_p_model[i]) = log(marginal_likelihood[i]) + log(prior[i]) -
    #                             log(sum_over_all_j( marginal_likelihood[j] * prior[j] ))
    #
    # and note the helpful R function matrixStats::logSumExp, where
    #
    #   logSumExp(lx) == log(sum(exp(lx))
    #
    # which, for lx == log(x), means
    #
    #   logSumExp(lx) == log(sum(x))
    #
    # so we will use
    #
    #   log(marginal_likelihood[j] * prior[j]) = log(marginal_likelihood[j]) +
    #                                            log(prior[j])

    d[, log_prior_times_lik :=
            log_marginal_likelihood + log_prior_p_model]
    d[, log_sum_prior_times_lik_all_models :=
            matrixStats::logSumExp(d$log_prior_times_lik)]
    d[, log_posterior_p_model :=
            log_prior_times_lik - log_sum_prior_times_lik_all_models]
    d[, posterior_p_model := exp(log_posterior_p_model)][]

    if (!detail) {
        # Remove working unless the user wants it
        d[, log_prior_p_model := NULL]
        # d[, log_prior_times_lik := NULL]
        # d[, log_sum_prior_times_lik_all_models := NULL]
        d[, log_posterior_p_model := NULL][]
    }

    # print(d)
    return(d)
}


stanfunc$sampled_values_from_stanfit <- function(
        fit,
        parname,
        method = c("extract", "manual", "as.matrix"))
{
    method <- match.arg(method)
    if (method == "manual") {
        # 1. Laborious hand-crafted way.
        n_chains <- slot(fit, "sim")$chains
        n_warmup <- slot(fit, "sim")$warmup
        sampled_values <- NULL
        for (c in 1:n_chains) {
            n_save <- slot(fit, "sim")$n_save[c]
            new_values <- slot(fit, "sim")$samples[[c]][parname][[1]][(n_warmup+1):n_save]
            sampled_values <- c(sampled_values, new_values)
        }
    } else if (method == "extract") {
        # 2. The way it's meant to be done.
        ex <- rstan::extract(fit, permuted=TRUE)
        if (!(parname %in% names(ex))) {
            stop("No such parameter: ", parname)
        }
        sampled_values <- ex[[parname]]
    } else if (method == "as.matrix") {
        # 3. Another...
        m <- as.matrix(fit)
        if (!(parname %in% colnames(m))) {
            stop("No such parameter: ", parname)
        }
        sampled_values <- m[,parname]
    } else {
        stop("Bad method")
    }
    return(sampled_values)
}


stanfunc$summary_by_par_regex <- function(fit, pars=NULL, par_regex=NULL, ...)
{
    # help("summary,stanfit-method")
    if (is.null(pars)) {
        s <- rstan::summary(fit, ...)
    } else {
        s <- rstan::summary(fit, pars=pars, ...)
    }
    # This summary object, s, has members:
    #   summary = overall summar
    #   c_summary = per-chain summary
    ss <- s$summary
    parnames <- rownames(ss)
    ss <- data.table(ss)
    ss$parameter <- parnames
    setcolorder(ss, c(ncol(ss), 1:(ncol(ss) - 1)))  # make last move to first
    # Optionally, filter on a regex
    if (!is.null(par_regex)) {
        ss <- ss[grepl(par_regex, ss$parameter), ]
    }
    return(ss)
}


stanfunc$annotated_parameters <- function(
        fit,
        pars = NULL,
        ci = c(0.025, 0.975),
        probs = c(0.025, 0.25, 0.50, 0.75, 0.975),
        par_regex = NULL,
        annotate = TRUE,
        ...
    )
{
    if (length(ci) != 2) {
        stop("Bad ci parameter")
    }
    if (!ci[1] %in% probs || !ci[2] %in% probs) {
        stop("Elements of ci must be in probs")
    }
    initial_probs <- probs
    if (annotate) {
        probs <- c(probs, c(0.0005, 0.9995,
                0.005, 0.995,
                0.025, 0.975,
                0.05, 0.95))
    } else {
        probs <- initial_probs
    }
    probs <- sort(unique(probs))
    s <- stanfunc$summary_by_par_regex(fit, pars=pars, probs=probs,
                                       par_regex=par_regex)
    # Find nonzero parameters (confidence intervals exclude zero)

    get_colname <- function(prob) {
        return(paste(prob * 100, "%", sep=""))
    }

    nonzero_at <- function(lower, upper) {
        lower_name = get_colname(lower)
        upper_name = get_colname(upper)
        return(
            sign(s[, lower_name, with=FALSE]) ==
                sign(s[, upper_name, with=FALSE]) &
            s[, lower_name, with=FALSE] != 0 &
            s[, upper_name, with=FALSE] != 0
        )
    }

    s[, nonzero := nonzero_at(ci[1], ci[2])][]

    if (annotate) {
        p_001 <- nonzero_at(0.0005, 0.9995)
        p_01 <- nonzero_at(0.005, 0.995)
        p_05 <- nonzero_at(0.025, 0.975)
        p_1 <- nonzero_at(0.05, 0.95)
        s[
            ,
            annotation := ifelse(p_001, "***",
                                 ifelse(p_01, "**",
                                        ifelse(p_05, "*",
                                               ifelse(p_1, ".", ""))))
        ][]
    }
    return(s)
}


stanfunc$nonzero_parameters <- function(fit, annotate=FALSE, ...)
{
    s <- stanfunc$annotated_parameters(fit=fit, annotate=annotate, ...)
    s <- s[nonzero == TRUE]  # restrict
    return(s)
}


#==============================================================================
# Running in parallel
#==============================================================================

stanfunc$parallel_stan <- function(
        file = NULL,
        code = "",
        data,
        cores = parallel:detectCores(),
        chains = 8,
        iter = 2000,
        warmup = floor(iter/2),
        seed = 1234)
{
    warning("stanfunc$parallel_stan: DEPRECATED; superseded by developments to rstan")
    cat("parallel_stan: cores=", cores,
        ", chains=", chains,
        ", iter=", iter,
        ", seed=", seed,
        "\n",
        sep="")

    cat("--- Step 1: compile the model (and run it once, very briefly, ignoring its output)\n")
    f1 <- stan(file = file,
               model_code = code,
               data = data,
               chains = 1,
               iter = 1,
               seed = seed,
               chain_id = 1)
    # sflist1 = list(f1)

    cat("--- Step 2: run more chains in parallel\n")
    sflist2 <- mclapply(
        1:chains,
        mc.cores = cores,
        function(i) {
            stan(fit = f1,
                 data = data,
                 chains = 1,
                 iter = iter,
                 warmup = warmup,
                 seed = seed,
                 chain_id = i)
        }
        # , refresh = -1
    )
    # ... passing the same seed to all chains follows example(sflist2stanfit)
    # ... important to use the same seed but different chain_id when executing in parallel: http://stackoverflow.com/questions/12848168/will-rstan-run-on-a-supercomputer

    sflist <- sflist2
    cat("--- Finished.\n")
    return(sflist2stanfit(sflist))
}

stanfunc$load_or_run_stan_old <- function(data, code, file, forcerun = FALSE)
{
    warning("stanfunc$load_or_run_stan_old: DEPRECATED; superseded by developments to rstan")
    if (!forcerun && file.exists(file)) {
        cat("Loading Stan model from file:", file, "\n")
        load(file)
    } else {
        cat("Running Stan model\n")
        fit = stanfunc$parallel_stan(code, data)
        cat("--- Saving Stan model to file:", file, "\n")
        save(fit, file=file)
    }
    return(fit)
}

stanfunc$parallel_stan_reuse_fit <- function(f1, data,
                                             cores = detectCores(),
                                             chains = 8,
                                             iter = 2000, seed = 1234)
{
    warning("stanfunc$parallel_stan_reuse_fit: DEPRECATED; superseded by developments to rstan")
    cat("parallel_stan_reuse_fit: cores=", cores,
        ", chains=", chains,
        ", iter=", iter,
        ", seed=", seed,
        "\n",
        sep="")

    cat("--- Reusing existing model\n")
    sflist2 <- mclapply(
        1:chains,
        mc.cores = cores,
        function(i) {
            stan(fit = f1, data = data,
                 iter = iter, seed = seed, chains = 1, chain_id = i)
        }
    )
    sflist <- sflist2
    cat("--- Finished.\n")
    return(sflist2stanfit(sflist))
}


#==============================================================================
# LOOKING AT OUTPUT
#==============================================================================

stanfunc$save_plots_from_stanfit <- function(
    fit,
    parfile = "teststan_parameters.pdf",
    tracefile = "teststan_trace.pdf",
    pairfile = "teststan_pairs.pdf")
{
    cat("Plotting parameters to", parfile, "\n")
    pdf(file = parfile)
    plot(fit)
    dev.off()
    cat("Plotting trace to", tracefile, "\n")
    pdf(file = tracefile)
    traceplot(fit) # options include: pars, inc_warmup
    dev.off()
    cat("Plotting pairs to", pairfile, "\n")
    pdf(file = pairfile)
    pairs(fit)
    dev.off()
}

stanfunc$quick_summary_stanfit <- function(
        fit, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
{
    print(fit, digits_summary = 5, probs = probs)
}

stanfunc$calculate_mode <- function(sampled_values)
{
    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    my_density$x[which(my_density$y == max_density)]
}

stanfunc$density_at_sub <- function(my_density, value)
{
    # Known exactly?
    if (value %in% my_density$x) {
        # cat("density_at: exact\n")
        return( my_density$y[ my_density$x == value ] )
    }
    # Out of range?
    if (value < min(my_density$x) || value > max(my_density$x) ) {
        cat("density_at: out of range\n")
        return(NA)
    }
    # Otherwise, interpolate:
    # cat("density_at: interpolating\n")
    lower_x <- max(my_density$x[my_density$x < value])
    upper_x <- min(my_density$x[my_density$x > value])
    lower_d <- my_density$y[my_density$x == lower_x]
    upper_d <- my_density$y[my_density$x == upper_x]
    proportion <- (value - lower_x) / (upper_x - lower_x)
    return(lower_d + proportion * (upper_d - lower_d))
}

stanfunc$density_at <- function(sampled_values, values)
{
    my_density <- density(sampled_values)
    result <- NULL
    for (v in values) {
        result <- c(result, density_at_sub(my_density, v))
    }
    return(result)
}

stanfunc$cum_density_between_two_values <- function(sampled_values,
                                                    lower, upper)
{
    my_ecdf <- ecdf(sampled_values)
    my_ecdf(upper) - my_ecdf(lower)
}

stanfunc$find_value_giving_density <- function(sampled_values, target_density)
{
    dens <- density(sampled_values)
    finder <- function(x) {
        density_at_sub(dens, x) - target_density
    }
    uniroot()
}

stanfunc$find_value_giving_cum_density <- function(sampled_values, cum_density)
{
    cdf <- ecdf(sampled_values)
    find_root <- function(x) {
        cdf(x) - cum_density
    }
    search_range <- c(min(sampled_values), max(sampled_values))
    value <- uniroot(find_root, interval = search_range)$root
}

stanfunc$JUNK1 = "
calculate_hdi_from_sample_interpolating <- function(x, hdi_proportion = 0.95)
{
    # INCOMPLETE
    # x contains sampled values
    dens = density(x)
    cdf = ecdf(x)
    finder_density_between_values_equals <- function(lower, upper, target_cum_density) {
        # 0 when CDF(upper) - CDF(lower) = target_cum_density
        cdf(upper) - cdf(lower) - target_cum_density
    }
    finder_density_equal_at_upper_and_lower <- function(lower, upper) {
        density_at_upper = density_at_sub(dens, upper)
        density_at_lower = density_at_sub(dens, lower)
        density_at_upper - density_at_lower
    }
    # https://stat.ethz.ch/pipermail/r-help/2007-November/146688.html
    density_diff <- function(lower, level=0.95) {
        plower = density_at_sub(dens, lower)
        pupper = plower + level
        # ...
    }
    # ...
}
"

stanfunc$calculate_hdi_from_sample_piecewise <- function(x, hdi_proportion = 0.95)
{
    # WORKS, BUT USE coda::HPDinterval instead
    # x contains sampled values
    # ... the shortest interval for which the difference in the empirical cumulative density function values of the endpoints is the nominal probability
    # http://stats.stackexchange.com/questions/18533/find-probability-density-intervals
    x <- sort(x)
    # http://www.sumsar.net/best_online/js/js_mcmc.js
    n <- length(x)
    ci_nbr_of_points <- floor(n * hdi_proportion) # want this many samples in the HDI
    min_width_ci <- c(min(x), max(y)) # initialize
    for (i in 1:(n - ci_nbr_of_points)) {
        ci_width <- x[i + ci_nbr_of_points] - x[i]
        if (ci_width < min_width_ci[2] - min_width_ci[1]) {
            min_width_ci <- c( x[i], x[i + ci_nbr_of_points] )
        }
    }
    return(min_width_ci)
    # We want an HDI such that
    #       CDF(upper) - CDF(lower) = hdi_proportion
    # and   PDF(upper) = PDF(lower)
}

stanfunc$HDIofMCMC = function(sampleVec, credMass = 0.95)
{
    # Krushke, p628, HDIofMCMC.R
    # Computes highest density interval from a sample of representative values,
    #   estimated as shortest credible interval.
    # Arguments:
    #  sampleVec
    #    is a vector of representative values from a probability distribution.
    # credMass
    #    is a scalar between 0 and 1, indicating the mass within the credible
    #    interval that is to be estimated.
    # Value:
    #  HDIlim is a vector containing the limits of the HDI
    sortedPts <- sort( sampleVec )
    ciIdxInc <- floor( credMass * length( sortedPts ) )
    nCIs <- length( sortedPts ) - ciIdxInc
    ciWidth <- rep( 0 , nCIs )
    for (i in 1:nCIs) {
        ciWidth[i] <- sortedPts[i + ciIdxInc] - sortedPts[i]
    }
    HDImin <- sortedPts[which.min(ciWidth)]
    HDImax <- sortedPts[which.min(ciWidth) + ciIdxInc]
    HDIlim <- c(HDImin, HDImax)
    return(HDIlim)
}

stanfunc$hdi_via_coda <- function(sampled_values, hdi_proportion = 0.95)
{
    hdi_limits_matrix <- coda::HPDinterval(as.mcmc(sampled_values),
                                           prob = hdi_proportion)
    return(c(hdi_limits_matrix[1, "lower"], hdi_limits_matrix[1, "upper"]))
}

stanfunc$hdi_via_lme4 <- function(sampled_values, hdi_proportion = 0.95)
{
    hdi_limits_matrix <- lme4::HPDinterval(as.matrix(sampled_values),
                                           prob = hdi_proportion)
    return(c( hdi_limits_matrix[1, "lower"], hdi_limits_matrix[1, "upper"]))
}

stanfunc$compare_hdi_methods <- function(sampled_values, hdi_proportion)
{
    cat("RNC:\n")
    print(calculate_hdi_from_sample_piecewise(sampled_values, hdi_proportion))
    cat("Krushke:\n")
    print(HDIofMCMC(sampled_values, hdi_proportion))
    cat("coda:\n")
    print(hdi_via_coda(sampled_values, hdi_proportion))
    cat("lme4:\n")
    print(hdi_via_lme4(sampled_values, hdi_proportion))
}

# Method chooser!
stanfunc$hdi <- function(sampled_values, hdi_proportion = 0.95)
{
    HDIofMCMC(sampled_values, hdi_proportion)
    # hdi_via_coda(sampled_values, hdi_proportion)
    # calculate_hdi_from_sample_piecewise(sampled_values, hdi_proportion)
}

stanfunc$interval_includes <- function(interval, testval,
                                       lower_inclusive = TRUE,
                                       upper_inclusive = TRUE)
{
    # Ensure ordered from low to high:
    if (interval[2] < interval[1]) {
        interval <- c(interval[2], interval[1])
    }
    lowertest <- ifelse(lower_inclusive,
                        interval[1] <= testval,
                        interval[1] < testval)
    uppertest <- ifelse(upper_inclusive,
                        testval <= interval[2],
                        testval < interval[2])
    return(lowertest && uppertest)
}

stanfunc$interval_excludes <- function(interval, testval,
                                       lower_inclusive = TRUE,
                                       upper_inclusive = TRUE)
{
    !stanfunc$interval_includes(interval, testval,
                                lower_inclusive = lower_inclusive,
                                upper_inclusive = upper_inclusive)
}

stanfunc$hdi_proportion_excluding_test_value <- function(
        x, test_value = 0, largest_such_interval = TRUE, debug = FALSE)
{
    # cruddy method!

    # NOTE ALSO: neither the lower bound nor the upper bound of an HDI
    # move monotonically as the HDI proportion is changed (because the
    # distribution can be asymmetrical).

    width_accuracy <- 0.001  # 0.1%

    if (largest_such_interval) {
        startval <- 1
        endval <- 0
        width_accuracy <- -width_accuracy
        stoptest <- interval_excludes
    }
    else {
        startval <- 0
        endval <- 1
        stoptest <- interval_includes
    }
    prev_width <- startval

    for (width in seq(startval, endval, width_accuracy)) {
        if (width == 1) next  # or HDI will be invalid (infinite)
        interval <- hdi(x, width)
        current_interval_fails <- stoptest(interval, test_value)
        if (debug) cat("testing proportion ", width,
                       ", interval: ", interval,
                       ", fails? ", current_interval_fails,
                       "\n", sep="")
        if (current_interval_fails) {
            # current interval fails, so return the previous
            return(prev_width)
        }
        prev_width <- width
    }
    return(endval)
}

stanfunc$plot_density_function <- function(
        sampled_values,
        parname,
        test_value = 0,
        quantile_probs = c(0.025, 0.5, 0.975),
        hdi_proportion = 0.95,
        histogram_breaks = 50,
        digits = 3,
        colour_quantiles = "gray",
        colour_mean = "black",
        colour_mode = "lightgrey",
        colour_hdi = "darkgreen",
        lty_quantiles = 3,
        lty_mean = 1,
        lty_mode = 1,
        lty_hdi = 1,
        colour_density = "blue",
        show_hdi_proportion_excluding_test_value = FALSE,
        show_quantiles = TRUE,
        show_mean = TRUE,
        show_mode = TRUE,
        show_hdi = TRUE,
        ypos_quantiles = 1.15,
        ypos_mean = 0.6,
        ypos_mode = 0.4)
{
    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    q <- quantile(sampled_values, probs=quantile_probs)
    my_mean <- mean(sampled_values)
    my_mode <- calculate_mode(sampled_values)
    my_ecdf <- ecdf(sampled_values)
    hdi_percent <- hdi_proportion * 100
    #debug_quantity(sampled_values)

    hdi_limits <- hdi(sampled_values, hdi_proportion)

    central_proportion_excluding_test_value <- 1 - 2 * my_ecdf(test_value)

    cat("\nParameter:", parname, "\n")
    cat("Mean:", my_mean, "\n")
    cat("Mode:", my_mode, "\n")
    cat("Quantiles:\n")
    print(q)
    cat("Central proportion excluding test value of", test_value, ":",
        central_proportion_excluding_test_value, "\n")
    cat(hdi_percent, "% HDI:\n")
    print(hdi_limits)
    if (show_hdi_proportion_excluding_test_value) {
        cat(
            "HDI proportion excluding test value of"
            , test_value
            , ":"
            , hdi_proportion_excluding_test_value(sampled_values, test_value)
            , "\n"
        )
    }
    #cat("Empirical CDF plotted\n")
    #plot(my_ecdf)

    cat("Plotting posterior distribution, with ", hdi_percent, "% HDI\n")
    plot(
        my_density$x,
        my_density$y,
        xlab = parname,
        ylab = "Density",
        main = paste("Posterior distribution of ", parname, sep=""),
        type = "l",
        bty = "n",
        col = colour_density,
        ylim = c(0, max_density * 1.2)
    )
    ypos_quantiles_upper <- max_density * ypos_quantiles
    ypos_quantiles_lower <- max_density * (ypos_quantiles - 0.05)
    ypos_quantiles_linetop <- max_density * (ypos_quantiles - 0.10)
    ypos_mean_upper <- max_density * ypos_mean
    ypos_mean_lower <- max_density * (ypos_mean - 0.05)
    ypos_mode_upper <- max_density * ypos_mode
    ypos_mode_lower <- max_density * (ypos_mode - 0.05)
    ypos_baseline <- max_density * -0.05

    # quantiles
    if (show_quantiles) {
        for (i in 1:length(q)) {
            lines(
                x = c(q[i], q[i]),
                y = c(ypos_baseline, ypos_quantiles_linetop),
                col = colour_quantiles,
                lty = lty_quantiles)
            text(q[i], ypos_quantiles_upper,
                 prettyNum(q[i], digits=digits), col = colour_quantiles)
            text(q[i], ypos_quantiles_lower,
                 paste(quantile_probs[i] * 100, "%", sep=""),
                 col = colour_quantiles)
        }
    }
    # mean
    if (show_mean) {
        lines(
            x = c(my_mean, my_mean),
            y = c(ypos_baseline, max_density),
            col = colour_mean,
            lty = lty_mean)
        text(my_mean, ypos_mean_upper,
             prettyNum(my_mean, digits=digits), col = colour_mean)
        text(my_mean, ypos_mean_lower,
             "(mean)", col = colour_mean)
    }
    # mode
    if (show_mode) {
        lines(
            x = c(my_mode, my_mode),
            y = c(ypos_baseline, max_density),
            col = colour_mode,
            lty = lty_mode)
        text(my_mode, ypos_mode_upper,
             prettyNum(my_mode, digits=digits), col = colour_mode)
        text(my_mode, ypos_mode_lower, "(mode)", col = colour_mode)
    }
    # HDI
    if (show_hdi) {
        density_at_hdi <- density_at(sampled_values, hdi_limits)
        # these will be imprecise (based on sampled density); the HDI itself is precise
        # So for visual reasons we take an approximate "water level":
        mean_density_at_hdi <- mean(density_at_hdi)
        lines(
            x = hdi_limits,
            y = density_at_hdi,
            lty = lty_hdi,
            col = colour_hdi
        )
        lines(
            x = c(hdi_limits[1], hdi_limits[1]),
            y = c(ypos_baseline, density_at_hdi[1]),
            lty = lty_hdi,
            col = colour_hdi
        )
        lines(
            x = c(hdi_limits[2], hdi_limits[2]),
            y = c(ypos_baseline, density_at_hdi[2]),
            lty = lty_hdi,
            col = colour_hdi
        )
        ypos_hdi_text <- mean_density_at_hdi + max_density * -0.05
        ypos_hdi_nums <- mean_density_at_hdi + max_density * 0.05
        text(mean(hdi_limits), ypos_hdi_text,
             paste(hdi_percent, "% HDI", sep=""), col = colour_hdi)
        text(hdi_limits[1], ypos_hdi_nums,
             prettyNum(hdi_limits[1], digits=digits), col = colour_hdi)
        text(hdi_limits[2], ypos_hdi_nums,
             prettyNum(hdi_limits[2], digits=digits), col = colour_hdi)
    }
}

stanfunc$ggplot_density_function <- function(
        sampled_values,
        parname,
        test_value = 0,
        quantile_probs = c(0.025, 0.5, 0.975),
        hdi_proportion = 0.95,
        histogram_breaks = 50,
        digits = 3,
        colour_quantiles = "gray",
        colour_mean = "black",
        colour_mode = "lightgrey",
        colour_hdi = "darkgreen",
        lty_quantiles = "dotted",
        lty_mean = "solid",
        lty_mode = "dashed",
        lty_hdi = "solid",
        colour_density = "blue",
        show_hdi_proportion_excluding_test_value = FALSE,
        show_quantiles = TRUE,
        show_mean = TRUE,
        show_mode = TRUE,
        show_hdi = TRUE,
        ypos_quantiles = 1.15,
        ypos_mean = 0.6,
        ypos_mode = 0.4,
        theme = theme_bw())
{
    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    q <- quantile(sampled_values, probs=quantile_probs)
    my_mean <- mean(sampled_values)
    my_mode <- calculate_mode(sampled_values)
    my_ecdf <- ecdf(sampled_values)
    hdi_percent <- hdi_proportion * 100

    hdi_limits <- hdi(sampled_values, hdi_proportion)

    central_proportion_excluding_test_value <- 1 - 2 * my_ecdf(test_value)

    df <- data.frame(x = my_density$x, y=my_density$y)
    p <- (
        ggplot(df, aes(x, y))
        + theme
        + geom_line(colour=colour_density)
        + xlab(parname)
        + ylab("Density")
        + ggtitle(paste("Posterior distribution of ", parname, sep=""))
        + ylim(0, max_density * 1.2)
    )

    ypos_quantiles_upper <- max_density * ypos_quantiles
    ypos_quantiles_lower <- max_density * (ypos_quantiles - 0.05)
    ypos_quantiles_linetop <- max_density * (ypos_quantiles - 0.10)
    ypos_mean_upper <- max_density * ypos_mean
    ypos_mean_lower <- max_density * (ypos_mean - 0.05)
    ypos_mode_upper <- max_density * ypos_mode
    ypos_mode_lower <- max_density * (ypos_mode - 0.05)
    ypos_baseline <- 0

    # quantiles
    if (show_quantiles) {
        for (i in 1:length(q)) {
            p <- (p
                + geom_vline(xintercept = q[i], colour = colour_quantiles,
                             linetype = lty_quantiles)
                + annotate("text", x = q[i], y = ypos_quantiles_upper,
                           label = prettyNum(q[i], digits = digits),
                           colour = colour_quantiles)
                + annotate("text", x = q[i], y = ypos_quantiles_lower,
                           label = paste(quantile_probs[i] * 100, "%", sep=""),
                           colour = colour_quantiles)
            )
        }
    }
    # mean
    if (show_mean) {
        p <- (p
            + geom_vline(xintercept = my_mean, colour = colour_mean,
                         linetype = lty_mean)
            + annotate("text", x = my_mean, y = ypos_mean_upper,
                       label = prettyNum(my_mean, digits = digits),
                       colour = colour_mean)
            + annotate("text", x = my_mean, y = ypos_mean_lower,
                       label = "(mean)", colour = colour_mean)
        )
    }
    # mode
    if (show_mode) {
        p <- (p
            + geom_vline(xintercept = my_mode, colour = colour_mode,
                         linetype = lty_mode)
            + annotate("text", x = my_mode, y = ypos_mode_upper,
                       label = prettyNum(my_mode, digits=digits),
                       colour = colour_mode)
            + annotate("text", x = my_mode, y = ypos_mode_lower,
                       label = "(mode)", colour = colour_mode)
        )
    }
    # HDI
    if (show_hdi) {
        density_at_hdi <- density_at(sampled_values, hdi_limits)
        # these will be imprecise (based on sampled density); the HDI itself is precise
        # So for visual reasons we take an approximate "water level":
        mean_density_at_hdi <- mean(density_at_hdi)
        ypos_hdi_text <- mean_density_at_hdi + max_density * -0.05
        ypos_hdi_nums <- mean_density_at_hdi + max_density * 0.05
        hdidf <- data.frame(
            x = c(hdi_limits[1], hdi_limits[1], hdi_limits[2], hdi_limits[2]),
            y = c(ypos_baseline, density_at_hdi[1], density_at_hdi[2], ypos_baseline)
        )
        print(hdidf)
        p <- (p
            + geom_line(
                data = hdidf,
                aes = aes(x = x, y = y),
                colour = colour_hdi
            )
            + annotate("text", x = mean(hdi_limits), y = ypos_hdi_text,
                       label = paste(hdi_percent, "% HDI", sep=""),
                       colour = colour_hdi)
            + annotate("text", x = hdi_limits[1], y = ypos_hdi_nums,
                       label = prettyNum(hdi_limits[1], digits = digits),
                       colour = colour_hdi)
            + annotate("text", x = hdi_limits[2], y = ypos_hdi_nums,
                       label = prettyNum(hdi_limits[2], digits = digits),
                       colour = colour_hdi)
        )
    }
    return(p)
}

stanfunc$plot_multiple_stanfit_parameters <- function(fit, parnames, ...)
{
    npar <- length(parnames)
    nside <- ceiling(sqrt(npar))
    par(mfrow = c(nside, nside))
    for (i in 1:npar) {
        stanfunc$test_specific_parameter_from_stanfit(fit, parnames[i], ...)
    }
}

stanfunc$plot_all_stanfit_parameters <- function(fit, ...)
{
    parnames <- stanfunc$get_all_parameters_from_stanfit(fit)
    stanfunc$plot_multiple_stanfit_parameters(fit, parnames, ...)
}

stanfunc$points_to_mm <- function(pts)
{
    pts * 0.352777778
}

stanfunc$plot_multiple_stanfit_parameters_vstack <- function(
        fit,
        params,  # list( list(name=name1, desc=desc1), list(name=name2, desc=desc2)...)
            # ... inner bit being a list because c() can't hold expressions properly
        inner_hdi_proportion = 0.90,
        outer_hdi_proportion = 0.95,
        xlab = bquote(
            paste(
                "mean"
                %+-%
                .(inner_hdi_proportion * 100),
                "/",
                .(outer_hdi_proportion * 100),
                "% HDI"
            )
        ),
        ylab = "",
        title = "Parameter value",
        compare_to = 0,
        theme = theme_bw(),
        reverse_sign = FALSE,  # flip the sign of all dependent variables
        show_hdi_proportion_excluding_comparison = FALSE,
        hdi_proportion_fontsize_points = 8,
        colour_hdi = TRUE)
{
    parnames <- sapply(params, function(x) x$name)
    pardesc <- sapply(params, function(x) {
        item <- ifelse(is.null(x$desc), x$name, x$desc)
        names(item) <- x$name
        # ... because ggplot scale_y_discrete wants a named character vector:
        # http://docs.ggplot2.org/0.9.3/discrete_scale.html
        return(item)
    } )

    colourmap <- c(
        # specify names, or scale_colour_manual shuffles them
        # (factor order is not the important thing)
        #
        # name (LHS): for the data frame
        # value (RHS): the ggplot colour

        "grey" = "grey50" # can adjust here too
        , "black" = "black"
        , "orange" = "orange"
        , "red" = "red"
    )

    n <- length(parnames)
    d <- data.frame(
        parname = parnames
        , yval = n:1 # top to bottom
        , innerhdi_colour = factor("black", names(colourmap))
        , outerhdi_colour = factor("grey", names(colourmap))
        , main_colour = factor("black", names(colourmap))
        , hdiprop_excluding_comparison = NA
        , hdiprop_x = NA
        , inner_hdi_lower = NA
        , inner_hdi_upper = NA
        , outer_hdi_lower = NA
        , outer_hdi_upper = NA
    )

    # Ensure the order is respected, and top-to-bottom
    d$parname <- factor(d$parname, levels = rev(d$parname), ordered = TRUE)

    show_hdi_exclusion_proportions <- (
        !is.na(compare_to) && show_hdi_proportion_excluding_comparison)

    for (i in 1:n) {
        parname <- as.character(d$parname[i])
        sampled_values <- sampled_values_from_stanfit(fit, parname)
        if (is.null(sampled_values)) {
            stop(paste("No values for parameter:", parname))
        }
        # ... the as.character() is vital!
        if (reverse_sign) {
            sampled_values <- -sampled_values
        }

        inner_hdi_limits <- hdi(sampled_values, inner_hdi_proportion)
        d$inner_hdi_lower[i] <- inner_hdi_limits[1]
        d$inner_hdi_upper[i] <- inner_hdi_limits[2]

        outer_hdi_limits <- hdi(sampled_values, outer_hdi_proportion)
        d$outer_hdi_lower[i] <- outer_hdi_limits[1]
        d$outer_hdi_upper[i] <- outer_hdi_limits[2]

        d$mean[i] <- mean(sampled_values)

        d$median[i] <- quantile(sampled_values, probs = c(0.5))

        if (!is.na(compare_to)) {
            d$inner_hdi_excludes_zero[i] <- interval_excludes(inner_hdi_limits, 0)
            d$outer_hdi_excludes_zero[i] <- interval_excludes(outer_hdi_limits, 0)

            if (colour_hdi) {
                if (d$inner_hdi_excludes_zero[i]) {
                    d$innerhdi_colour[i] <- "orange"
                    d$main_colour[i] <- "orange"
                }
                if (d$outer_hdi_excludes_zero[i]) {
                    d$innerhdi_colour[i] <- "red"
                    d$outerhdi_colour[i] <- "red"
                    d$main_colour[i] <- "red"
                }
            }
            if (show_hdi_exclusion_proportions) {
                d$hdiprop_excluding_comparison[i] <- paste(
                    100 * hdi_proportion_excluding_test_value(sampled_values,
                                                              compare_to)
                    , "%"
                    , sep = ""
                )
            }
        }
    }
    max_x <- max(d$mean, d$outer_hdi_upper, na.rm = TRUE)
    min_x <- min(d$mean, d$outer_hdi_lower, na.rm = TRUE)
    x_range <- max_x - min_x
    HDIPROP_SPACE <- ifelse(show_hdi_exclusion_proportions, 0.1, 0)
    EXTRASPACE <- 0.1
    d$hdiprop_x <- min_x - HDIPROP_SPACE * x_range

    p <- (
        ggplot(data = d, aes(y = parname, x = mean) )
        # layer up from bottom to top; can't use ifelse() for elements, for some reason
    )
    if (!is.na(compare_to)) {
        p <- p + geom_vline(xintercept = 0)
    }
    p <- (
        p
        + scale_y_discrete(labels = pardesc)
        + geom_errorbarh(aes(xmin = outer_hdi_lower, xmax = outer_hdi_upper,
                             colour = outerhdi_colour))
        + geom_errorbarh(aes(xmin = inner_hdi_lower, xmax = inner_hdi_upper,
                             colour = innerhdi_colour))
        + geom_point(size = 3, aes(colour = main_colour))
    )
    if (show_hdi_exclusion_proportions) {
        p <- p + geom_text(
            aes(x = hdiprop_x, y = parname, label = hdiprop_excluding_comparison),
            size = points_to_mm(hdi_proportion_fontsize_points)
        )
        # https://groups.google.com/forum/#!topic/ggplot2-dev/N7oaqvjiKzg
    }
    p <- (
        p
        + coord_cartesian(xlim = c(min_x - (HDIPROP_SPACE + EXTRASPACE) * x_range,
                                   max_x + (EXTRASPACE) * x_range))
        + xlab(xlab)
        + ylab(ylab)
        + scale_colour_manual(values = colourmap, guide = FALSE)
        + theme
        + ggtitle(title)
    )
    return(p)
}

stanfunc$plot_all_stanfit_parameters_vstack <- function(fit, ...)
{
    parnames <- stanfunc$get_all_parameters_from_stanfit(fit)
    stanfunc$plot_multiple_stanfit_parameters_vstack(fit, parnames, ...)
}

stanfunc$generate_par_with_indices <- function(pn, pd)
{
    #debug_quantity(pn)
    #debug_quantity(pd)
    ndims <- length(pd)
    if (ndims == 0) {
        return(pn)
    }
    indexlist <- list()
    for (d in 1:ndims) {
        indexlist <- append(indexlist, list(x = seq(1:pd[d])))
    }
    #cat("indexlist: \n"); print(indexlist)
    indices <- expand.grid(indexlist)
    #cat("indices: \n"); print(indices)
    parnames <- NULL
    for (r in 1:nrow(indices)) {
        name <- paste(pn, "[", sep="")
        for (c in 1:ncol(indices)) {
            if (c > 1) name <- paste(name, ",", sep="")
            name <- paste(name, indices[r,c], sep="")
        }
        name <- paste(name, "]", sep="")
        parnames <- c(parnames, name)
    }
    #cat("parnames: \n"); print(parnames)
    return(parnames)
}

stanfunc$get_all_parameters_from_stanfit <- function(fit)
{
    parnames_without_indices <- slot(fit, "model_pars")
    pardims <- slot(fit, "par_dims")
    parnames <- NULL
    for (i in 1:length(pardims)) {
        pn <- parnames_without_indices[i]
        pd <- pardims[[i]]
        parnames <- c(parnames, generate_par_with_indices(pn, pd))
    }
    return(parnames)
}

stanfunc$get_parameter_mean_from_stanfit <- function(fit, parname)
{
    sampled_values <- stanfunc$sampled_values_from_stanfit(fit, parname)
    return(mean(sampled_values))
}

stanfunc$get_parameter_means_from_stanfit <- function(fit, parnames)
{
    return(aaply(parnames, 1, .fun = function(x) {
        stanfunc$get_parameter_mean_from_stanfit(fit, x)
    }))
}

stanfunc$test_specific_parameter_from_stanfit <- function(fit, parname, ...)
{
    # ??rstan
    # ?stan
    # ?rstan::print.stanfit -- NOTE OPTIONS: probs (quartiles of interest), digits_summary (sig. digits)
    # ?extract
    # ?traceplot

    # showClass("stanfit")
    # methods(class = "stanfit")

    # slot(fit, "model_pars")
    # print(summary(fit))

    # CAN'T GET HELP:
    # ?rstan::stanfit-class -- https://code.google.com/p/stan/source/browse/rstan/rstan/man/stanfit-class.Rd?name=web
    # ?rstan::plot-methods

    # names(slot(fit, "sim"))
    # ...  [1] "samples" "chains" "iter" "thin" "warmup" "n_save" "warmup2" "permutation" "pars_oi" "dims_oi" "fnames_oi" "n_flatnames"
    # for chain 1:
    # slot(fit, "sim")$samples[[1]]$"group_mean_reinf_rate[1]"
    # quantile(slot(fit, "sim")$samples[[1]]$"group_mean_reinf_rate[1]", probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

    # the opposite of quantile is ecdf:
    # x = slot(fit, "sim")$samples[[1]]$"gmd_side_stickiness_max"
    # EF = ecdf(x)
    # EF(0)

    # NOTE, however, that this contains all runs
    sampled_values <- stanfunc$sampled_values_from_stanfit(fit, parname)
    stanfunc$plot_density_function(sampled_values, parname, ...)
}

stanfunc$ggplot_specific_parameter_from_stanfit <- function(fit, parname, ...)
{
    sampled_values <- stanfunc$sampled_values_from_stanfit(fit, parname)
    return( stanfunc$ggplot_density_function(sampled_values, parname, ...) )
}

stanfunc$extract_all_means_from_stanfit <- function(fit)
{
    parnames <- stanfunc$get_all_parameters_from_stanfit(fit)
    means <- stanfunc$get_parameter_means_from_stanfit(fit, parnames)
    names(means) <- parnames
    return(means)
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("stanfunc" %in% search()) detach("stanfunc")
attach(stanfunc)  # subsequent additions not found, so attach at the end
