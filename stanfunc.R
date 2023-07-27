# stanfunc.R

# Note re data.table:
# ... trailing [] to prevent "doesn't print first time" bug:
# https://stackoverflow.com/questions/32988099/data-table-objects-not-printed-after-returned-from-function
# https://github.com/Rdatatable/data.table/blob/master/NEWS.md#bug-fixes-5


tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    bridgesampling,
    coda,
    data.table,
    ggplot2,
    HDInterval,
    matrixStats,
    parallel,
    reshape,
    rstan,
    stringr
)
rm(tmp_require_package_namespace)


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

stanfunc <- new.env()

stanfunc$DEFAULT_CHAINS <- 8
stanfunc$DEFAULT_ITER <- 2000
stanfunc$DEFAULT_INIT <- "0"  # the Stan default, "random", uses the range -2 to +2
stanfunc$DEFAULT_SEED <- 1234  # for consistency across runs

stanfunc$DEFAULT_HIGH_RHAT_THRESHOLD <- 1.1
    #   If this threshold for R-hat is exceeded, warnings are shown. A value
    #   of 1.2 is a typical threshold and 1.1 is a stringent criterion (Brooks
    #   and Gelman 1998, doi:10.1080/10618600.1998.10474787, p. 444).

stanfunc$DEFAULT_HDI_METHOD <- "HDInterval"
stanfunc$DEFAULT_HDI_PROPORTION <- 0.95


#==============================================================================
# Core functions for e.g. rstan 2.16.2:
#==============================================================================

stanfunc$load_or_run_stan <- function(
        data,
        fit_filename,
        model_name,
        file = NULL,
        model_code = "",
        save_stancode_filename = NULL,
        save_data_filename = NULL,
        save_cpp_filename = NULL,
        save_code_filename = NULL,  # Deprecated; see below.
        forcerun = FALSE,
        chains = stanfunc$DEFAULT_CHAINS,
        iter = stanfunc$DEFAULT_ITER,
        init = stanfunc$DEFAULT_INIT,
        seed = stanfunc$DEFAULT_SEED,
        cache_filetype = c("rds", "rda"),
        ...)
{
    # If a fit has been saved to a cache file, load and return it.
    # Otherwise, run a Stan model (and save it to the cache file).
    #
    # Args:
    #   data
    #       Stan data, a list.
    #   fit_filename
    #       Filename of cache for fit.
    #   model_name
    #       Textual name of the model.
    #   file
    #       Filename for Stan code source. (Alternative to "model_code".)
    #   model_code
    #       Text of Stan code. (Alternative to "file".)
    #   save_stancode_filename
    #       Optional filename to save Stan code (as text).
    #   save_data_filename
    #       Optional filename to save the data, using saveRDS().
    #   save_cpp_filename
    #       Optional filename to save the Stan-generated C++ code. Unnecessary;
    #       both Stan and C++ code is extractable from the Stan fit.
    #   save_code_filename
    #       (DEPRECATED.) Old name for save_cpp_filename.
    #   forcerun
    #       Run Stan (and re-save the result) even if the cache exists.
    #   chains
    #       Number of chains, for Stan.
    #   iter
    #       Number of iterations, for Stan.
    #   init
    #       Method for initialization of parameters, for Stan.
    #       The Stan default, "random", uses the range -2 to +2.
    #   seed
    #       Random number generator seed, for Stan. For consistency across
    #       runs.
    #   cache_filetype
    #       Save as RDS (saveRDS/readRDS) or RDA (save/load)?
    #   ...
    #       Other arguments to rstan::stan(). Common potential parameters:
    #           control = list(
    #               adapt_delta = 0.99
    #           )
    #
    #       For adapt_delta, see e.g.
    #       - http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    #       - https://www.rdocumentation.org/packages/rstanarm/versions/2.14.1/topics/adapt_delta

    if (is.null(file) == (model_code == "")) {
        stop("Specify either 'file' or 'model_code' (and not both).")
    }

    cache_filetype <- match.arg(cache_filetype)
    if (!is.null(save_code_filename)) {
        if (!is.null(save_cpp_filename)) {
            stop("Can't specify both 'save_code_filename' (old) and 'save_cpp_filename' (new)")
        }
        save_cpp_filename <- save_code_filename
    }

    saving <- forcerun || !file.exists(fit_filename)

    # -------------------------------------------------------------------------
    # Save Stan code file, if requested
    # -------------------------------------------------------------------------
    # ... unnecessary; both Stan and C++ code is extractable from the Stan
    # fit; but helpful if the code compiles/executes but crashes with a
    # line number error.

    if (saving && !is.null(save_stancode_filename)) {
        if (model_code == "") {
            stop("Must specify 'model_code' to use 'save_stancode_filename'")
        }
        cat("--- Saving Stan code to file: ",
            save_stancode_filename, "...\n", sep = "")
        stancodefile <- file(save_stancode_filename)
        writeLines(model_code, stancodefile)
        close(stancodefile)
        cat("... saved\n")
    }

    # -------------------------------------------------------------------------
    # Save Stan data file, if requested. (RDS only.)
    # -------------------------------------------------------------------------

    if (saving && !is.null(save_data_filename)) {
        cat("--- Saving Stan data to file: ",
            save_data_filename, "...\n", sep = "")
        saveRDS(data, file = save_data_filename)
        cat("... saved\n")
    }

    # -------------------------------------------------------------------------
    # Save C++ code file, if requested
    # -------------------------------------------------------------------------

    if (saving && !is.null(save_cpp_filename)) {
        cat("--- Generating C++ code to save...\n")
        # Note that it distinguishes between 'file' being NULL (OK) or
        # missing (not).
        if (!is.null(file)) {
            stanc_result <- rstan::stanc(file = file)
        } else {
            stanc_result <- rstan::stanc(model_code = model_code)
        }
        cpp_code <- stanc_result$cppcode

        cat("--- Saving C++ code to file: ",
            save_cpp_filename, "...\n", sep = "")
        cppfile <- file(save_cpp_filename)
        writeLines(cpp_code, cppfile)
        close(cppfile)
        cat("... saved\n")
    }

    # -------------------------------------------------------------------------
    # Load fit or run Stan
    # -------------------------------------------------------------------------

    if (!saving) {
        # ---------------------------------------------------------------------
        # Load fit
        # ---------------------------------------------------------------------
        if (cache_filetype == "rds") {
            # .Rds
            cat("Loading Stan model fit from RDS file: ",
                fit_filename, "...\n", sep = "")
            fit <- readRDS(fit_filename)
        } else {
            # .Rda, .Rdata
            cat("Loading Stan model fit from RDA file: ",
                fit_filename, "...\n", sep = "")
            fit <- NULL  # so we can detect the change when we load
            load(fit_filename)  # assumes it will be called 'fit'
            if (class(fit) != "stanfit") {
                stop(paste("No stanfit object called 'fit' in file",
                           fit_filename))
            }
        }
        cat("... loaded\n")
    } else {
        # ---------------------------------------------------------------------
        # Run Stan
        # ---------------------------------------------------------------------
        n_cores_stan <- getOption("mc.cores")
        if (is.null(n_cores_stan)) {
            n_cores_stan <- 0
            # https://github.com/HenrikBengtsson/Wishlist-for-R/issues/7
        }
        n_cores_available <- parallel::detectCores()
        if (n_cores_stan < n_cores_available) {
            warning(paste0(
                "Stan is not set to use all available CPU cores; using ",
                n_cores_stan, " when ", n_cores_available,
                " are available; retry after issuing the command\n",
                "    options(mc.cores = parallel::detectCores())"
            ))
        }
        if (n_cores_stan <= 1) {
            warning("Running with a single CPU core; Stan may be slow")
        }

        cat(paste0(
            "--- Running Stan, model ", model_name,
            ", starting at ", Sys.time(), "...\n"
        ))

        # Stan now supports parallel operation directly
        # Note that it distinguishes between 'file' being NULL (OK) or
        # missing (not).
        if (!is.null(file)) {
            fit <- rstan::stan(
                file = file,
                model_name = model_name,
                data = data,
                chains = chains,
                iter = iter,
                init = init,
                seed = seed,
                ...
            )
        } else {
            fit <- rstan::stan(
                model_code = model_code,
                model_name = model_name,
                data = data,
                chains = chains,
                iter = iter,
                init = init,
                seed = seed,
                ...
            )
        }
        cat(paste("... Finished Stan run at", Sys.time(), "\n"))

        # ---------------------------------------------------------------------
        # Save fit
        # ---------------------------------------------------------------------
        if (cache_filetype == "rds") {
            # .Rds
            cat("--- Saving Stan model fit to RDS file: ",
                fit_filename, "...\n", sep = "")
            saveRDS(fit, file = fit_filename)  # load with readRDS()
        } else {
            # .Rda, .Rdata
            cat("--- Saving Stan model fit to RDA file: ",
                fit_filename, "...\n", sep = "")
            save(list = c("fit"), file = fit_filename)
        }
        cat("... saved\n")
    }

    return(fit)
}


stanfunc$load_or_run_bridge_sampler <- function(
    stanfit,
    filename,
    assume_stanfit_from_this_R_session = FALSE,
    file = NULL,
    model_code = "",
    data = NULL,
    cores = parallel::detectCores(),
    forcerun = FALSE,
    algorithm = NULL,  #
    ...)
{
    # Load (from a cache) or run bridge sampling using a Stan fit object.
    #
    # NOTE that the Stan model must have been adapted to use
    # bridgesampling-compatible methods (e.g. "target += lpdf...()... and other
    # adjustments for clipped parameters), to avoid dropping constants (as Stan
    # will via "~" sampling notation).
    #
    # Args:
    #   stanfit
    #       Stan fit object.
    #   filename
    #       Cache filename for bridgesampling results.
    #   assume_stanfit_from_this_R_session
    #       If the Stan fit was created in this R session, we can use the fit
    #       object directly; of not, we have to regenerate a model, which is a
    #       bit slower (or bridgesampling may crash).
    #   file
    #       Filename for Stan code source. (Alternative to "model_code".)
    #   model_code
    #       Text of Stan code. (Alternative to "file".)
    #   data
    #       Stan data, a list.
    #   cores
    #       Number of CPU cores to use.
    #   forcerun
    #       Run Stan (and re-save the result) even if the cache exists.
    #   algorithm
    #       Passed to rstan::stan(). See ?rstan:stan. For the rare occasions
    #       when you want "Fixed_param".
    #   ...
    #       Other arguments to bridgesampling::bridge_sampler().

    if (!forcerun && file.exists(filename)) {
        # ---------------------------------------------------------------------
        # Load
        # ---------------------------------------------------------------------
        cat("Loading bridge_sampler() fit from RDS file: ",
            filename, "...\n", sep = "")
        b <- readRDS(filename)
        cat("... loaded\n")
    } else {
        # ---------------------------------------------------------------------
        # Run
        # ---------------------------------------------------------------------
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
            if (is.null(file) == (model_code == "")) {
                stop("Specify either 'file' or 'model_code' (and not both).")
            }
            if (is.null(data)) {
                stop("data not specified")
            }
            if (!is.null(file)) {
                stanfit_model <- rstan::stan(
                    file = file,
                    data = data,  # if you use data = list(), it segfaults
                    chains = 1,
                    iter = 1,  # despite the bridgesampling help, iter = 0 causes an error
                    algorithm = algorithm
                )
            } else {
                stanfit_model <- rstan::stan(
                    model_code = model_code,
                    data = data,
                    chains = 1,
                    iter = 1,
                    algorithm = algorithm
                )
            }
            cat("... done\n")
        }
        cat(paste("--- Running bridge_sampler, starting at",
                  Sys.time(), "...\n"))
        b <- bridgesampling::bridge_sampler(
            samples = stanfit,
            stanfit_model = stanfit_model,
            cores = cores,
            ...
        )
        cat(paste("... Finished bridge_sampler run at", Sys.time(), "\n"))

        # ---------------------------------------------------------------------
        # Save
        # ---------------------------------------------------------------------
        cat("--- Saving bridge_sampler() fit to RDS file: ",
            filename, "...\n", sep = "")
        saveRDS(b, file = filename)  # load with readRDS()
        cat("... saved\n")
    }
    return(b)
}


stanfunc$load_or_run_vb <- function(
        data,
        vbfit_filename,
        model_name,
        file = NULL,
        model_code = "",
        forcerun = FALSE,
        init = stanfunc$DEFAULT_INIT,
        seed = stanfunc$DEFAULT_SEED,
        ...)
{
    if (is.null(file) == (model_code == "")) {
        stop("Specify either 'file' or 'model_code' (and not both).")
    }

    if (!forcerun && file.exists(vbfit_filename)) {
        # ---------------------------------------------------------------------
        # Load
        # ---------------------------------------------------------------------

        cat("Loading Stan VB fit from RDS file: ",
            vbfit_filename, "...\n", sep = "")
        vb_fit <- readRDS(vbfit_filename)
        cat("... loaded\n")

    } else {

        # ---------------------------------------------------------------------
        # Run
        # ---------------------------------------------------------------------
        cat(paste0("Running variational Bayes approximation to Stan model ",
                   model_name, ", starting at ", Sys.time(), "...\n"))

        cat("Building model...")
        if (!is.null(file)) {
            vb_model <- rstan::stan_model(file = file,
                                          model_name = model_name)
        } else {
            vb_model <- rstan::stan_model(model_code = model_code,
                                          model_name = model_name)
        }

        cat("Running VB...")
        vb_fit <- rstan::vb(
            object = vb_model,
            data = data,
            seed = seed,
            init = init,
            ...
        )

        cat(paste("... Finished Stan VB run at", Sys.time(), "\n"))

        # ---------------------------------------------------------------------
        # Save
        # ---------------------------------------------------------------------
        cat("--- Saving Stan model fit to RDS file: ",
            vbfit_filename, "...\n", sep = "")
        saveRDS(vb_fit, file = vbfit_filename)  # load with readRDS()
        cat("... saved\n")
    }

    return(vb_fit)
}


stanfunc$quickrun <- function(
    data,
    model_name,
    fit_cache_dir,
    file = NULL,
    model_code = "",
    forcerun = FALSE,
    chains = stanfunc$DEFAULT_CHAINS,
    iter = stanfunc$DEFAULT_ITER,
    init = stanfunc$DEFAULT_INIT,
    seed = stanfunc$DEFAULT_SEED,
    control = NULL,
    vb = FALSE,
    save_code = FALSE,
    FIT_SUFFIX = "_stanfit.rds",
    BRIDGE_SUFFIX = "_bridgesampling.rds",
    VBFIT_SUFFIX = "_stanvbfit.rds",
    CPP_SUFFIX = "_code.cpp",
    ...)
{
    # A shortcut to (a) run Stan normally or via variational Bayes (VB)
    # approximation, (b) if not using VB, run bridge sampling.
    #
    # Args:
    #   data
    #       Stan data, a list.
    #   model_name
    #       Textual name of the model.
    #   fit_cache_dir
    #       Directory in which to load/save cache information. Appropriate
    #       filenames are created from model_name.
    #   file
    #       Filename for Stan code source. (Alternative to "model_code".)
    #   model_code
    #       Text of Stan code. (Alternative to "file".)
    #   chains
    #       Number of chains, for Stan.
    #   iter
    #       Number of iterations, for Stan.
    #   init
    #       Method for initialization of parameters, for Stan.
    #       The Stan default, "random", uses the range -2 to +2.
    #   seed
    #       Random number generator seed, for Stan. For consistency across
    #       runs.
    #   control
    #       The Stan "control" parameter (a list), e.g. for adapt_delta. See
    #       above.
    #   vb
    #       Use quick-and-dirty variational Bayes approximation?
    #   save_code
    #       Save the C++ code? Unnecessary; both Stan and C++ code is
    #       extractable from the Stan fit.
    #   FIT_SUFFIX
    #       Suffix for building the filename for the (normal) fit cache.
    #   BRIDGE_SUFFIX
    #       Suffix for building the filename for the bridge sampling cache.
    #   VBFIT_SUFFIX
    #       Suffix for building the filename for the VB fit cache.
    #   CPP_SUFFIX
    #       Suffix for building the filename for C++ code.
    #   ...
    #       Additional parameters to rstan::stan().

    if (is.null(file) == (model_code == "")) {
        stop("Specify either 'file' or 'model_code' (and not both).")
    }

    # Note that C++ code is extractable from the Stan fit.
    fit_filename <- file.path(fit_cache_dir,
                              paste0(model_name, FIT_SUFFIX))
    bridge_filename <- file.path(fit_cache_dir,
                              paste0(model_name, BRIDGE_SUFFIX))
    vbfit_filename <- file.path(fit_cache_dir,
                                paste0(model_name, VBFIT_SUFFIX))
    if (save_code) {
        cpp_filename <- file.path(fit_cache_dir,
                                  paste0(model_name, CPP_SUFFIX))
    } else {
        cpp_filename <- NULL
    }

    result <- list(
        model_name = model_name,
        fit = NULL,
        bridge = NULL,
        shinystan = NULL,
        vb_fit = NULL
    )

    if (vb) {

        cat(paste0("Running variational Bayes approximation to Stan model ",
                   model_name, "...\n"))
        result$vb_fit <- stanfunc$load_or_run_vb(
            data = data,
            file = file,
            model_code = model_code,
            vbfit_filename = vbfit_filename,
            model_name = model_name,
            forcerun = forcerun,
            init = init,
            seed = seed
        )

    } else {

        cat(paste0("Running Stan model ", model_name, "...\n"))

        # Stan fit
        result$fit <- stanfunc$load_or_run_stan(
            file = file,
            data = standata,
            model_code = model_code,
            fit_filename = fit_filename,
            model_name = model_name,
            save_code_filename = cpp_filename,
            forcerun = forcerun,
            chains = chains,
            iter = iter,
            init = init,
            seed = seed,
            control = control,
            ...
        )

        # View the model in ShinyStan
        cat("Making ShinyStan object...\n")
        result$shinystan <- shinystan::as.shinystan(result$fit)
        cat("... made\n")
        # Use with: shinystan::launch_shinystan(result$shinystan)

        # Bridge sampling
        result$bridge <- stanfunc$load_or_run_bridge_sampler(
            stanfit = result$fit,
            filename = bridge_filename,
            file = file,
            model_code = model_code,
            data = standata
        )

    }

    return(result)
}


stanfunc$compare_model_evidence <- function(
        bridgesample_list_list,
        priors = NULL,
        detail = FALSE,
        rhat_warning_threshold = stanfunc$DEFAULT_HIGH_RHAT_THRESHOLD,
        rhat_par_exclude_regex = NULL,
        rhat_par_selected_regex = NULL)
{
    # Compare, using bridge sampling, multiple Stan fits.
    #
    # Args:
    #   bridgesample_list_list
    #       A list of lists. Each item is a list with names:
    #           name:
    #               the model name
    #           bridgesample:
    #               the output from the bridgesampling::bridge_sampler()
    #               function (an item of class bridge_list)
    #           stanfit (optional):
    #               a corresponding Stan fit
    #               ... useful to show e.g. maximum R-hat summaries
    #       (R note: if x is a list, then if x *doesn't* have item y, x$y ==
    #       NULL.)
    #
    #   new_quantile_functions:
    #       Optional, but can be a vector containing prior probabilities for
    #       each model.
    #
    #   detail:
    #       Keep the details used for intermediate calculations?
    #
    #   rhat_warning_threshold:
    #       If this threshold for R-hat is reached/exceeded, warnings are
    #       shown. A value of 1.2 is a typical threshold and 1.1 is a stringent
    #       criterion (Brooks and Gelman 1998,
    #       doi:10.1080/10618600.1998.10474787, p. 444).
    #
    #   rhat_par_exclude_regex:
    #       Regex for parameters to exclude from R-hat calculation.
    #
    # Notes:
    # - "marginal likelihood" is the same as "evidence" (e.g. Kruschke 2011
    #   p57-58)
    # - https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
    # - https://stackoverflow.com/questions/4227223/r-list-to-data-frame
    # - CHECK THE OUTPUT AGAINST, e.g.:
    #   bridgesampling::post_prob(b1, b2, b3, b4, b5, b6, model_names = paste("Model", 1:6))
    #   ... verified.

    d <- data.table(
        t(
            vapply(
                X = seq_along(bridgesample_list_list),
                FUN = function(y, i) {
                    item <- y[[i]]
                    if (is.null(item$stanfit)) {
                        max_rhat <- NA_real_
                    } else {
                        fit <- item$stanfit
                        max_rhat <- stanfunc$max_rhat(
                            fit,
                            par_exclude_regex = rhat_par_exclude_regex
                        )
                        max_rhat_selected <- stanfunc$max_rhat(
                            fit,
                            par_regex = rhat_par_selected_regex,
                            par_exclude_regex = rhat_par_exclude_regex
                        )
                    }
                    return(c(
                        i,  # index
                        item$name,  # model_name
                        item$bridgesample$logml,  # log_marginal_likelihood
                        max_rhat,  # max_rhat
                        max_rhat_selected  # max_rhat_selected
                    ))
                },
                FUN.VALUE = c("index" = NA_integer_,
                              "model_name" = NA_character_,
                              "log_marginal_likelihood" = NA_real_,
                              "max_rhat" = NA_real_,
                              "max_rhat_selected" = NA_real_),
                y = bridgesample_list_list
            )
        )
    )
    d[, index := as.numeric(index)]
    d[, log_marginal_likelihood := as.numeric(log_marginal_likelihood)]
    rhat_bad_label <- "WARNING: HIGH R-HAT"
    rhat_good_label <- "OK"
    d[, rhat_warning := ifelse(
        is.na(max_rhat),
        NA_character_,
        ifelse(max_rhat >= rhat_warning_threshold,
               rhat_bad_label, rhat_good_label)
    )]
    d[, rhat_selected_warning := ifelse(
        is.na(max_rhat_selected),
        NA_character_,
        ifelse(max_rhat_selected >= rhat_warning_threshold,
               rhat_bad_label, rhat_good_label)
    )]

    d[, model_rank := frank(-log_marginal_likelihood,
                            ties.method = "min")]  # "sports method"
    # ... bigger (less negative) is better
    # ... and rank() ranks from smallest (-> 1) to biggest, so want the reverse
    # ... and data.table::frank is quicker than rank (not that we care here!)

    n_models <- nrow(d)
    if (is.null(priors)) {
        # Flat new_quantile_functions
        d[, prior_p_model := 1/n_models]
    } else {
        # User-specified new_quantile_functions
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
    #                                 marginal_likelihood[i] * prior[i]
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
        d[, log_prior_times_lik := NULL]
        d[, log_sum_prior_times_lik_all_models := NULL]
        # d[, log_posterior_p_model := NULL][]
    }

    # print(d)
    return(d)
}


stanfunc$sampled_values_from_stanfit <- function(
        fit,
        parname,
        method = c("extract", "manual", "as.matrix"))
{
    # Extract sampled values from a Stan fit object, for a specific parameter.
    #
    # Args:
    #   fit
    #       The Stan fit.
    #   parname
    #       Name of the parameter
    #   method
    #       Options:
    #       - manual: Laborious hand-crafted way.
    #       - extract: The way it's meant to be done, via rstan::extract().
    #         The default.
    #       - as.matrix: By converting the fit to a matrix.

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
        ex <- rstan::extract(fit, permuted = TRUE)
        # Now, slightly tricky. For a plain-text parameter like "xyz", this
        # is simple. For something like "subject_k[1]", it isn't so simple,
        # because rstan::extract gives us proper structure.
        # Can also be e.g. parname[1,1], etc.
        # Grep with capture: https://stackoverflow.com/questions/952275/regex-group-capture-in-r-with-multiple-capture-groups

        PARAM_WITH_INDEX_REGEX <- "^(\\w+)\\[((?:\\d+,)*\\d+)\\]$"  # e.g. "somevar[3]", "blah[1,2]"
        # matches <- stringr::str_match("blah", PARAM_WITH_INDEX_REGEX)
        # matches <- stringr::str_match("blah[1]", PARAM_WITH_INDEX_REGEX)
        # matches <- stringr::str_match("blah[2,3]", PARAM_WITH_INDEX_REGEX)
        matches <- stringr::str_match(parname, PARAM_WITH_INDEX_REGEX)
        if (!is.na(matches[1])) {
            # parameter with index/indices e.g. "subject_k[3]", "blah[1,1]"
            parname_par <- matches[2]
            index_csv_numbers <- matches[3]
            indices <- as.integer(unlist(strsplit(index_csv_numbers, ",")))
            if (!(parname_par %in% names(ex))) {
                stop("No such parameter: ", parname)
            }
            sampled_array <- ex[[parname_par]]
            # ... for one index, sampled_array has indices [samplenum, parnum]
            #     so one can use sampled_values <- sampled_array[, parname_num]
            # ... but for two, dim(sampled_array) is e.g. c(8000, 3, 3); this
            #     means 8000 samples of a 3x3 array.
            # To retrieve them... see http://r.789695.n4.nabble.com/array-slice-notation-td902486.html
            arraydims <- dim(sampled_array)
            if (length(indices) != length(arraydims) - 1) {
                stop("Bad indices for parameter: ", parname,
                     ". Indices were: ", indices,
                     " and dimensions were: ", arraydims)
            }
            n_samples <- arraydims[1]
            slicelist <- c(list(1:n_samples), as.list(indices))
            # e.g. for param[3, 3], slicelist should be a list whose first
            # element is 1:8000 (for 8000 samples), whose second element is 3,
            # and whose third element is 3.
            sampled_values <- do.call("[", c(list(sampled_array), slicelist))
        } else {
            # e.g. "somevar"
            if (!(parname %in% names(ex))) {
                stop("No such parameter: ", parname)
            }
            sampled_values <- ex[[parname]]
        }

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


stanfunc$summary_data_table <- function(fit, ...)
{
    # Makes a data table from rstan::summary().
    #
    # See https://mc-stan.org/rstan/reference/stanfit-method-summary.html
    #
    # Args:
    #   ...
    #       Passed to rstan::summary().
    #       The "probs" argument is a vector of quantiles of interest (for each
    #       parameter).

    # help("summary,stanfit-method")
    s <- rstan::summary(fit, ...)
    # This summary object, s, has members:
    #   summary = overall summary
    #   c_summary = per-chain summary
    ss <- s$summary
    parnames <- rownames(ss)
    ss <- data.table(ss)
    ss$parameter <- parnames
    # Move the "parameters" column so it's first:
    setcolorder(ss, c(ncol(ss), 1:(ncol(ss) - 1)))  # make last move to first
    return(ss)
}


stanfunc$summary_by_par_regex <- function(fit,
                                          pars = NULL,
                                          par_regex = NULL,
                                          par_exclude_regex = NULL,
                                          ...)
{
    # Calls stanfunc$summary_data_table() for a subset of parameters.
    #
    # Extracting parameters can be slow, so we filter parameter names before
    # asking rstan to extract parameters.

    if (is.null(pars)) {
        pars <- names(fit)  # all parameter names; this is quick
    }
    # Apply inclusion regex:
    if (!is.null(par_regex)) {
        pars <- pars[grepl(par_regex, pars)]
    }
    if (!is.null(par_exclude_regex)) {
        pars <- pars[!grepl(par_exclude_regex, pars)]
    }
    if (length(pars) == 0) {
        stop("No parameters selected")
    }

    s <- stanfunc$summary_data_table(fit, pars = pars, ...)
    return(s)
}


stanfunc$params_with_high_rhat <- function(
        fit,
        threshold = stanfunc$DEFAULT_HIGH_RHAT_THRESHOLD,
        par_exclude_regex = NULL)
{
    # Returns rows from stanfunc$summary_by_par_regex() where the R-hat
    # ("convergence problem") value exceeds (is worse than) a threshold.

    s <- stanfunc$summary_by_par_regex(fit,
                                       par_exclude_regex = par_exclude_regex)
    return(s[Rhat >= threshold])
}


stanfunc$max_rhat <- function(fit,
                              par_regex = NULL,
                              par_exclude_regex = NULL)
{
    # Returns the maximum R-hat value for any parameter in the fit meeting the
    # filter criteria.

    s <- stanfunc$summary_by_par_regex(fit,
                                       par_regex = par_regex,
                                       par_exclude_regex = par_exclude_regex)
    return(max(s$Rhat))
}


stanfunc$annotated_parameters <- function(
        fit,
        pars = NULL,
        par_regex = NULL,
        par_exclude_regex = NULL,
        ci = c(0.025, 0.975),
        probs = c(0.025, 0.50, 0.975),
        annotate = TRUE,
        nonzero_as_hdi = TRUE,
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION,
        hdi_method = stanfunc$DEFAULT_HDI_METHOD,
        rhat_warning_threshold = stanfunc$DEFAULT_HIGH_RHAT_THRESHOLD,
        vb = FALSE
    )
{
    # Produces a summary table for a Stan fit, with these columns:
    #   parameter
    #       Parameter name.
    #   mean
    #   se_mean
    #   sd
    #       Usual meanings: posterior mean, standard error of the mean,
    #       standard deviation.
    #   2.5%, 97.5% (etc.)
    #       The exact quantile columns used are determined by "probs". These
    #       are quantiles provided by stanfunc$summary_by_par_regex() and thus
    #       ultimately by rstan::summary().
    #   n_eff
    #       Stan measure (number of effective samples).
    #   Rhat
    #       Stan measure of convergence.
    #   annotation
    #       Added if "annotate" is TRUE; see below.
    #   hdi_lower
    #   hdi_upper
    #       If nonzero_as_hdi is TRUE, these columns are provided.
    #       The hdi_lower and hdi_upper columns are calculated using
    #       stanfunc$hdi() according to the "hdi_proportion" and "hdi_method"
    #       parameters (q.v.).
    #   summary
    #       String with mean, 95% quantile interval, and R-hat.
    #       Formatted like: "–0.173 [–0.242, –0.093] (R=1.003)".
    #   rangetype
    #       QI (quantile interval) or HDI (highest density interval) used for
    #       "summary" (+/- "vs_zero") column.
    #   nonzero
    #       Whether the HDI (if nonzero_as_hdi is TRUE) or QI (if
    #       nonzero_as_hdi is FALSE) excludes zero.
    #   qi_vs_zero
    #       Whether the quantile interval (according to "ci" by Stan-provided
    #       quantiles) excludes zero. Added if "annotate" is TRUE; see below.
    #
    # Args:
    #   fit
    #       The Stan fit object.
    #   pars
    #       Named parameters to include. (If NULL, not restricted.)
    #   par_regex
    #       A regular expression that parameters must MATCH to be included.
    #   par_exclude_regex
    #       A regular expression that parameters must NOT MATCH to be included.
    #   ci
    #       Credible interval boundary probabilities, e.g. c(0.025, 0.975) for
    #       a 95% QI or HDI, maybe used for calculating the "nonzero" column
    #       (q.v.).
    #   probs
    #       All quantile probabilities to display. (The "ci" probabilities must
    #       be within "probs", but you can add any others.)
    #   annotate
    #       If true, adds an "annotation" column:
    #           *** if zero not in 99.9% QI (0.001 outside) -- not the HDI
    #           **  if zero not in 99% QI (0.01 outside) -- not the HDI
    #           *   if zero not in 95% QI (0.05 outside) -- not the HDI
    #           .   if zero not in 90% (0.1 outside) -- not the HDI
    #       (QI = quantile interval.)
    #   nonzero_as_hdi
    #       Use the HDI (see hdi_proportion, hdi_method) for the "nonzero"
    #       column? If false, uses the quantiles determined by "ci".
    #   hdi_proportion
    #       For HDI calculation: what HDI proportion (e.g. 0.95 for 95% HDI).
    #   hdi_method
    #       For HDI calculation: method of calculation; see stanfunc$hdi().
    #   rhat_warning_threshold:
    #       If this threshold for R-hat is reached/exceeded, warnings are
    #       shown. A value of 1.2 is a typical threshold and 1.1 is a stringent
    #       criterion (Brooks and Gelman 1998,
    #       doi:10.1080/10618600.1998.10474787, p. 444).
    #   vb
    #       Was the fit generated via variational Bayes (VB) approximation?
    #       (Such fits have no R-hat measure.)
    #
    # Changes:
    # - 2023-07-23: (a) changed default hdi_method from "kruschke_mcmc" to
    #   stanfunc$DEFAULT_HDI_METHOD, now "HDInterval"; (b) absorbed all
    #   functions from stanfunc$summarize_fit, so there's only one (they were
    #   nearly identical except the latter supported more filtering but only
    #   QIs); (c) add "rangetype" column; (d) remove 25%/75% quantiles from the
    #   default to save space.

    # -------------------------------------------------------------------------
    # Sort out arguments
    # -------------------------------------------------------------------------
    if (length(ci) != 2) {
        stop("Bad ci parameter")
    }
    ci <- sort(ci)
    ci_lower <- ci[1]
    ci_upper <- ci[2]
    initial_probs <- probs
    if (!ci_lower %in% probs || !ci_upper %in% probs) {
        stop("Elements of ci must be in probs (unless you set nonzero_as_hdi)")
    }
    if (annotate) {
        probs <- c(
            probs,
            c(
                0.0005, 0.9995,  # by comparison to p < 0.001 two-tailed
                0.005, 0.995,  # for "**", cf. p < 0.01 two-tailed
                0.025, 0.975,  # for "*", cf. p < 0.05 two-tailed
                0.05, 0.95  # for ".", cf. p < 0.1 two-tailed
            )
        )
    }
    probs <- sort(unique(probs))

    # -------------------------------------------------------------------------
    # Annotation helper functions
    # -------------------------------------------------------------------------
    f <- function(x) {
        y <- sprintf("%.3f", x)
        y <- gsub("-", "–", y)
        return(y)
    }
    get_p_colname <- function(prob) {
        # Get column name for quantile.
        return(paste0(prob * 100, "%"))
    }

    qi_nonzero_at <- function(lower, upper) {
        # Does our QI range exclude zero?
        lower_colname <- get_p_colname(lower)
        upper_colname <- get_p_colname(upper)
        return(
            0 < s[, lower_colname, with = FALSE] |
            s[, upper_colname, with = FALSE] < 0
        )
    }

    # -------------------------------------------------------------------------
    # Filtered fit object
    # -------------------------------------------------------------------------
    s <- stanfunc$summary_by_par_regex(
        fit,
        pars = pars,
        par_regex = par_regex,
        par_exclude_regex = par_exclude_regex,
        probs = probs
    )

    # -------------------------------------------------------------------------
    # Annotate "nonzero" status by QUANTILE INTERVAL (do they exclude zero?)
    # -------------------------------------------------------------------------
    if (annotate) {
        p_001 <- qi_nonzero_at(0.0005, 0.9995)  # 0.001
        p_01 <- qi_nonzero_at(0.005, 0.995)  # 0.01
        p_05 <- qi_nonzero_at(0.025, 0.975)  # 0.05
        p_1 <- qi_nonzero_at(0.05, 0.95)  # 0.1
        s[
            ,
            qi_vs_zero := ifelse(
                p_001,
                "***",
                ifelse(
                    p_01,
                    "**",
                    ifelse(
                        p_05,
                        "*",
                        ifelse(p_1, ".", "")
                    )
                )
            )
        ]
    }

    # -------------------------------------------------------------------------
    # Determine HDI if required
    # -------------------------------------------------------------------------
    if (nonzero_as_hdi) {
        s[, hdi_lower := NA_real_]
        s[, hdi_upper := NA_real_]
        for (rownum in 1:nrow(s)) {
            parname <- s[rownum, parameter]
            values <- stanfunc$sampled_values_from_stanfit(fit, parname)
            # User-requested HDI
            hdi_pair <- stanfunc$hdi(
                values,
                hdi_proportion = hdi_proportion,
                method = hdi_method
            )
            s[rownum, hdi_lower := hdi_pair[1]]
            s[rownum, hdi_upper := hdi_pair[2]]
        }
    }
    # It would be very slow to calculate all the HDIs required for the asterisk
    # notation as below.

    # -------------------------------------------------------------------------
    # Summary column
    # -------------------------------------------------------------------------
    if (nonzero_as_hdi) {
        ci_lower_colname <- "hdi_lower"
        ci_upper_colname <- "hdi_upper"
    } else {
        ci_lower_colname <- get_p_colname(ci_lower)
        ci_upper_colname <- get_p_colname(ci_upper)
    }
    if (vb) {
        # Variational Bayes approximation; no R-hat
        maketextcol <- function(m, a, b) {
            paste0(f(m), " [", f(a), ", ", f(b), "]")
        }
        s[, summary := maketextcol(
            mean,
            s[[ci_lower_colname]],
            s[[ci_upper_colname]]
        )]
    } else{
        # Full Stan fit
        maketextcol <- function(m, a, b, r) {
            rhat_warning <- ifelse(
                r >= rhat_warning_threshold,
                " !",
                ""
            )
            paste0(
                f(m), " [", f(a), ", ", f(b), "] (R=", f(r), rhat_warning, ")"
            )
        }
        s[, summary := maketextcol(
            mean,
            s[[ci_lower_colname]],
            s[[ci_upper_colname]],
            s[["Rhat"]]
        )]
    }
    # -------------------------------------------------------------------------
    # "Nonzero" by the user's preferred measure
    # -------------------------------------------------------------------------
    s[, nonzero :=
            0 < s[, ci_lower_colname, with = FALSE] |
            s[, ci_upper_colname, with = FALSE] < 0
    ]

    # -------------------------------------------------------------------------
    # Make the range type clear
    # -------------------------------------------------------------------------
    s[, rangetype := ifelse(nonzero_as_hdi, "HDI", "QI")]

    # -------------------------------------------------------------------------
    # Tidy up
    # -------------------------------------------------------------------------
    hidden_probs <- setdiff(probs, initial_probs)  # in former, not latter
    for (remove_prob in hidden_probs) {
        remove_colname <- get_p_colname(remove_prob)
        # cat(paste("Removing column", remove_colname, "\n"))
        s[, (remove_colname) := NULL]
    }

    # -------------------------------------------------------------------------
    # Done
    # -------------------------------------------------------------------------
    s <- s[]  # for data table display bug
    return(s)
}


stanfunc$summarize_fit <- stanfunc$annotated_parameters


stanfunc$nonzero_parameters <- function(fit, annotate = FALSE, ...)
{
    # Snapshot of the results of stanfunc$annotated_parameters() for which
    # "nonzero" is TRUE. See that function for help.

    s <- stanfunc$annotated_parameters(fit = fit, annotate = annotate, ...)
    s <- s[nonzero == TRUE][]  # restrict
    return(s)
}


stanfunc$code_from_stanfit <- function(fit)
{
    # Returns the Stan source code from a fitted model.

    return(fit@stanmodel@model_code)
}


stanfunc$cpp_from_stanfit <- function(fit)
{
    # Returns the C++ code from a fitted model.

    return(fit@stanmodel@model_cpp$model_cppcode)
}


#==============================================================================
# Running in parallel
#==============================================================================

stanfunc$parallel_stan <- function(
        file = NULL,
        code = "",
        data,
        cores = parallel:detectCores(),
        chains = stanfunc$DEFAULT_CHAINS,
        iter = stanfunc$DEFAULT_ITER,
        warmup = floor(iter/2),
        seed = stanfunc$DEFAULT_SEED)
{
    # DEPRECATED. Very old helper function, before Stan supported parallel
    # processing. Runs rstan::stan() for a model -- once to compile, then
    # several times in parallel, using parallel::mclapply() and
    # rstan::sflist2stanfit() to combine the results.

    warning("stanfunc$parallel_stan: DEPRECATED; superseded by developments to rstan")
    cat("parallel_stan: cores = ", cores,
        ", chains = ", chains,
        ", iter = ", iter,
        ", seed = ", seed,
        "\n",
        sep = "")

    cat("--- Step 1: compile the model (and run it once, very briefly, ignoring its output)\n")
    f1 <- rstan::stan(file = file,
               model_code = code,
               data = data,
               chains = 1,
               iter = 1,
               seed = seed,
               chain_id = 1)
    # sflist1 = list(f1)

    cat("--- Step 2: run more chains in parallel\n")
    sflist2 <- parallel::mclapply(
        1:chains,
        mc.cores = cores,
        function(i) {
            rstan::stan(fit = f1,
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
    return(rstan::sflist2stanfit(sflist))
}


stanfunc$load_or_run_stan_old <- function(data, code, file, forcerun = FALSE)
{
    # DEPRECATED. Very old helper function, before Stan supported parallel
    # processing. Cached (RDA) version of stanfunc$parallel_stan().

    warning("stanfunc$load_or_run_stan_old: DEPRECATED; superseded by developments to rstan")
    if (!forcerun && file.exists(file)) {
        cat("Loading Stan model from file:", file, "\n")
        load(file)
    } else {
        cat("Running Stan model\n")
        fit <- stanfunc$parallel_stan(code, data)
        cat("--- Saving Stan model to file:", file, "\n")
        save(fit, file = file)
    }
    return(fit)
}


stanfunc$parallel_stan_reuse_fit <- function(f1, data,
                                             cores = detectCores(),
                                             chains = stanfunc$DEFAULT_CHAINS,
                                             iter = stanfunc$DEFAULT_ITER,
                                             seed = stanfunc$DEFAULT_SEED)
{
    # DEPRECATED. Very old helper function, before Stan supported parallel
    # processing. Runs Stan in parallel, using a precompiled model.

    warning("stanfunc$parallel_stan_reuse_fit: DEPRECATED; superseded by developments to rstan")
    cat("parallel_stan_reuse_fit: cores = ", cores,
        ", chains = ", chains,
        ", iter = ", iter,
        ", seed = ", seed,
        "\n",
        sep = "")

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
    # Saves plots of parameters, traceplots, and pairs, from a Stan fit.

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
    # Of limited use! Prints a Stan fit for selected probabilities and at 5
    # decimal places.

    print(fit, digits_summary = 5, probs = probs)
}


stanfunc$calculate_mode <- function(sampled_values)
{
    # Calculate the mode of sampled values, via a kernel density method.

    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    my_density$x[which(my_density$y == max_density)]
}


stanfunc$density_at_sub <- function(my_density, value)
{
    # Given a kernel density estimate ["my_density", being the output of
    # density()], estimate the density at a particular value. If the density is
    # known exactly, that is returned; otherwise, the density is interpolated
    # linearly between the two adjacent values for which it's available.

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
    # Estimates the probability density of "sampled values" at the values
    # "values", interpolating if necessary.

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
    # Using an empirical cumulative density function via ecdf(), provide the
    # cumulative density between two boundary values.

    my_ecdf <- ecdf(sampled_values)
    my_ecdf(upper) - my_ecdf(lower)
}


# stanfunc$find_value_giving_density <- function(sampled_values, target_density)
# {
#     # UNUSED, LIKELY BROKEN.
#
#     dens <- density(sampled_values)
#     finder <- function(x) {
#         density_at_sub(dens, x) - target_density
#     }
#     uniroot()
# }


stanfunc$find_value_giving_cum_density <- function(sampled_values, cum_density)
{
    # Find the value (exact or estimated) for which the cumulative density of
    # "sampled_values" is "cum_density".

    cdf <- ecdf(sampled_values)
    # The output of ecdf() is a FUNCTION that takes one [e.g. 5] or many values
    # [e.g. c(2, 5)] and returns the estimated CDF at the values given.

    find_root <- function(x) {
        # x is a single value
        cdf(x) - cum_density
    }
    # Starting interval is the full range of sampled values:
    search_range <- c(min(sampled_values), max(sampled_values))
    # The uniroot() function searches the interval provided for a root (zero)
    # of the function provided.
    value <- uniroot(find_root, interval = search_range)$root
}


stanfunc$JUNK1 <- "
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
    density_diff <- function(lower, level = 0.95) {
        plower = density_at_sub(dens, lower)
        pupper = plower + level
        # ...
    }
    # ...
}
"


stanfunc$calculate_hdi_from_sample_piecewise <- function(
        x,
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION)
{
    # Calculates an HDI.
    #
    # Args:
    #   x
    #       Sampled values.
    #   hdi_proportion:
    #       E.g. 0.95% for a 95% HDI.
    #
    # Returns:
    #   The shortest interval for which the difference in the empirical
    #   cumulative density function values of the endpoints is the nominal
    #   probability.
    #
    # Adapted from a Javascript version at Rasmus Bååth's blog:
    # - https://www.sumsar.net/best_online/js/js_mcmc.js
    # - https://www.sumsar.net/best_online/js
    #
    # See:
    # http://stats.stackexchange.com/questions/18533/find-probability-density-intervals

    x <- sort(x)
    n <- length(x)
    ci_nbr_of_points <- floor(n * hdi_proportion) # want this many samples in the HDI
    min_width_ci <- c(min(x), max(x)) # initialize
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


stanfunc$HDIofMCMC <- function(
        sampleVec,
        credMass = stanfunc$DEFAULT_HDI_PROPORTION)
{
    # From Kruschke (2011) "Doing Bayesian Data Analysis", p628, HDIofMCMC.R.
    # No changes apart from trivial formatting for house style.
    #
    # Computes highest density interval from a sample of representative values,
    # estimated as shortest credible interval.
    #
    # Arguments:
    #   sampleVec
    #       is a vector of representative values from a probability
    #       distribution.
    #   credMass
    #       is a scalar between 0 and 1, indicating the mass within the
    #       credible interval that is to be estimated.
    #
    # Return value:
    #   HDIlim is a vector containing the limits of the HDI

    sortedPts <- sort(sampleVec)
    ciIdxInc <- floor(credMass * length(sortedPts))
    nCIs <- length(sortedPts) - ciIdxInc
    ciWidth <- rep(0 , nCIs)
    for (i in 1:nCIs) {
        ciWidth[i] <- sortedPts[i + ciIdxInc] - sortedPts[i]
    }
    HDImin <- sortedPts[which.min(ciWidth)]
    HDImax <- sortedPts[which.min(ciWidth) + ciIdxInc]
    HDIlim <- c(HDImin, HDImax)
    return(HDIlim)
}


stanfunc$TEST_HDI_OF_MCMC <- '
    # See Kruschke (2011) p41, p628.
    set.seed(1234)
    n = 20000
    symmetric_y <- rnorm(n, mean = 0, sd = 1)
    symmetric_d <- density(symmetric_y)
    symmetric_hdi <- stanfunc$HDIofMCMC(symmetric_y)  # -1.975671  1.904936
    plot(symmetric_d)
    abline(v = symmetric_hdi[1])
    abline(v = symmetric_hdi[2])
    asymmetric_y <- rgamma(n, shape = 2, scale = 2)
    asymmetric_d <- density(asymmetric_y)
    asymmetric_hdi <- stanfunc$HDIofMCMC(asymmetric_y)  #
    plot(asymmetric_d)
    abline(v = asymmetric_hdi[1])
    abline(v = asymmetric_hdi[2])
'


stanfunc$hdi_via_coda <- function(
        sampled_values,
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION)
{
    # Calculates a highest density interval (HDI) via coda::HPDinterval();
    # see ?coda::HPDinterval.
    #
    # Its source code is at e.g. https://rdrr.io/cran/coda/src/R/HPDinterval.R

    # The coda::as.mcmc() function essentially just puts values into a data
    # object that the Coda library likes.
    mcmc_obj <- coda::as.mcmc(sampled_values)
    hdi_limits_matrix <- coda::HPDinterval(mcmc_obj, prob = hdi_proportion)
    # ... Sometimes crashes with
    # "Error in dimnames(x)[[2]] : subscript out of bounds"
    # for perfectly valid-looking data that works with other methods.

    return(c(hdi_limits_matrix[1, "lower"], hdi_limits_matrix[1, "upper"]))
}

stanfunc$hdi_via_hdinterval <- function(
        sampled_values,
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION)
{
    # Calculates an HDI using the HDInterval package, by Kruschke et al.
    i <- HDInterval::hdi(sampled_values, credMass = hdi_proportion)
    return(c(i["lower"], i["upper"]))
}


stanfunc$hdi_via_lme4 <- function(
        sampled_values,
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION)
{
    # Calculates a highest density interval (HDI) via lme4::HPDinterval();
    # see ?lme4::HPDinterval. However, while this function was in lme4 in 2013
    # (e.g.
    # https://si.biostat.washington.edu/sites/default/files/modules/lme4.pdf),
    # it has gone by 2023 (e.g.
    # https://cran.r-project.org/web/packages/lme4/lme4.pdf).

    hdi_limits_matrix <- lme4::HPDinterval(as.matrix(sampled_values),
                                           prob = hdi_proportion)
    return(c(hdi_limits_matrix[1, "lower"], hdi_limits_matrix[1, "upper"]))
}


stanfunc$compare_hdi_methods <- function(
        sampled_values,
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION)
{
    # Reports HDI estimates using a variety of methods.

    cat("Bååth:\n")
    print(stanfunc$calculate_hdi_from_sample_piecewise(sampled_values, hdi_proportion))

    cat("coda:\n")
    print(stanfunc$hdi_via_coda(sampled_values, hdi_proportion))
    # Sometimes crashes for no obvious reason; see stanfunc$hdi_via_coda().

    cat("HDInterval:\n")
    print(stanfunc$hdi_via_hdinterval(sampled_values, hdi_proportion))

    cat("Kruschke (2011 book version):\n")
    print(stanfunc$HDIofMCMC(sampled_values, hdi_proportion))

    # REMOVED FROM lme4 by 2023
    # cat("lme4:\n")
    # print(stanfunc$hdi_via_lme4(sampled_values, hdi_proportion))
}


stanfunc$hdi <- function(
    sampled_values,
    hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION,
    method = c("HDInterval", "coda", "kruschke_mcmc", "baath"))
{
    # Calculates a highest density interval (HDI) from sampled values,
    # according to a variety of methods.
    #
    # Args:
    #   sampled_values
    #       The values
    #   hdi_proportion
    #       E.g. 0.95% for a 95% HDI.
    #   method:
    #       Method to use:
    #       - HDInterval
    #           Use stanfunc$hdi_via_hdinterval(); q.v.
    #       - kruschke_mcmc
    #           Use stanfunc$HDIofMCMC(); q.v.
    #       - coda
    #           Use coda::HPDinterval(coda::as.mcmc(...)).
    #       - baath
    #           Use stanfunc$calculate_hdi_from_sample_piecewise(); q.v.
    #       - (REMOVED) lme4
    #           Use lme4::HPDinterval(), since removed from the lme4 package.
    #
    # Changes:
    # - 2023-07-27: default changed from "kruschke_mcmc" to "HDInterval".
    #   Should make no difference but more eyes on the HDInterval package.
    #   (This value here should also match stanfunc$DEFAULT_HDI_METHOD.)

    # Method chooser!
    method <- match.arg(method)
    if (method == "HDInterval") {
        return(stanfunc$hdi_via_hdinterval(sampled_values, hdi_proportion))
    } else if (method == "kruschke_mcmc") {
        return(stanfunc$HDIofMCMC(sampled_values, hdi_proportion))
    } else if (method == "coda") {
        return(stanfunc$hdi_via_coda(sampled_values, hdi_proportion))
    } else if (method == "baath") {
        return(stanfunc$calculate_hdi_from_sample_piecewise(sampled_values, hdi_proportion))
    # } else if (method == "lme4") {
    #    return(stanfunc$hdi_via_lme4(sampled_values, hdi_proportion))
    } else {
        stop("Bad method")
    }
}


stanfunc$interval_includes <- function(interval, testval,
                                       lower_inclusive = TRUE,
                                       upper_inclusive = TRUE)
{
    # Does the interval, specified as a 2-tuple such as c(0, 1), include the
    # test value?
    #
    # The lower_inclusive and upper_inclusive parameters determine whether the
    # interval's boundaries are treated as inclusive or exclusive.

    stopifnot(length(interval) == 2)
    # Ensure ordered from low to high:
    interval <- sort(interval)

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
    # Does the interval, specified as a 2-tuple such as c(0, 1), exclude the
    # test value?
    #
    # The lower_inclusive and upper_inclusive parameters determine whether the
    # interval's boundaries are treated as inclusive or exclusive.

    !stanfunc$interval_includes(interval, testval,
                                lower_inclusive = lower_inclusive,
                                upper_inclusive = upper_inclusive)
}


stanfunc$hdi_proportion_excluding_test_value <- function(
        x, test_value = 0, largest_such_interval = TRUE, debug = FALSE,
        width_accuracy = 0.001)
{
    # "What HDI proportion excludes the test value?"
    #
    # cruddy method! (Inefficient.)
    #
    # Args:
    #   x
    #       Sampled values.
    #   test_value
    #       A test value
    #   largest_such_interval
    #       - If TRUE, work from 1 (100% HDI) down to 0 (0% HDI) and stop when
    #         the HDI no longer includes the value.
    #       - If FALSE, work from 0 (0% HDI) up to 1 (100% HDI) and stop (just
    #         before) when the HDI includes the value.
    #   width_accuracy
    #       Accuracy (step size) for search; default is 0.001 = 0.1%.
    #
    # Returns:
    #   Approximately the largest HDI proportion (e.g. 0.87 for the 87% HDI,
    #   0.95 for the 95% HDI) that excludes test_value -- either a high-end
    #   estimate (if largest_such_interval is TRUE) or a low-end estimate (if
    #   largest_such_interval is FALSE).
    #
    # NOTE ALSO: neither the lower bound nor the upper bound of an HDI move
    # monotonically as the HDI proportion is changed (because the distribution
    # can be asymmetrical). Thus, an ascending approach and a descending
    # approach can give different answers; the largest_such_interval parameter
    # determines the direction of search.

    if (largest_such_interval) {
        # Work from 1 (100% HDI) down to 0 (0% HDI) and stop when the HDI
        # no longer includes the value.
        startval <- 1
        endval <- 0
        width_accuracy <- -width_accuracy
        stoptest <- stanfunc$interval_excludes
    } else {
        # Work from 0 (0% HDI) up to 1 (100% HDI) and stop when the HDI
        # includes the value.
        startval <- 0
        endval <- 1
        stoptest <- stanfunc$interval_includes
    }
    prev_width <- startval

    for (width in seq(startval, endval, width_accuracy)) {
        if (width == 1) next  # or HDI will be invalid (infinite)
        interval <- hdi(x, width)
        current_interval_fails <- stoptest(interval, test_value)
        if (debug) cat("testing proportion ", width,
                       ", interval: ", interval,
                       ", fails? ", current_interval_fails,
                       "\n", sep = "")
        if (current_interval_fails) {
            # Current interval fails...
            # - if largest_such_interval == TRUE, that means the current
            #   interval EXCLUDES the test value (but the previous, which
            #   was larger, included it). We return the previous, which is
            #   an upper-bound estimate of the true target.
            # - if largest_such_interval == FALSE, that means the current
            #   interval INCLUDES the test value (but the previous, which
            #   was smaller, excluded it) -- so return the previous. This
            #   gives a lower-bound estimate.
            return(prev_width)
        }
        prev_width <- width
    }
    return(endval)
}


stanfunc$TEST_HDI_PROP_EXC_VALUE <- "
    set.seed(0)
    x <- rbeta(1000, 2, 5)  # asymmetrical
    hist(x)  # show distribution
    test_value <- 0.6  # in upper tail
    stanfunc$hdi_proportion_excluding_test_value(x, test_value, largest_such_interval = TRUE, debug = FALSE)
    stanfunc$hdi_proportion_excluding_test_value(x, test_value, largest_such_interval = FALSE, debug = FALSE)
"


stanfunc$plot_density_function <- function(
        sampled_values,
        parname = deparse(substitute(sampled_values)),
        test_value = 0,
        quantile_probs = c(0.025, 0.5, 0.975),
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION,
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
    # Explore the distribution of a variable, via a plot and a report.
    # See also stanfunc$ggplot_density_function().
    #
    # Includes:
    # - a density plot
    # - mean, mode, median or other quantiles (plot, report)
    # - central interval excluding a target value (report)
    # - a specified HDI (plot, report)
    # - largest HDI excluding a target value (report)
    #
    # The plot is labelled "posterior distribution" for convenience, but the
    # posterior nature is from what you feed in! It just plots the
    # distribution.
    #
    # For asymmetric distributions, this illustrates the difference between the
    # "central interval" (or "quantile interval") and the "highest density
    # interval" (HDI). A good illustration is at
    # https://cran.r-project.org/web/packages/HDInterval/HDInterval.pdf, for
    # symmetric (quantile) intervals versus HDIs.
    #
    # Args:
    #   sampled_values
    #       The values.
    #   parname
    #       Decorative parameter name, for the plot.
    #   test_value
    #       Value to test "against", graphically (e.g. zero for "is it
    #       non-zero"?).
    #   quantile_probs
    #       Quantiles to plot.
    #   hdi_proportion
    #       HDI to plot (e.g. 0.95 for a 95% HDI).
    #   digits
    #       Number of digits for number formatting.
    #   colour_...
    #       Colours for quantiles, mean, mode, and HDI plotting.
    #   lty_...
    #       Line types for quantiles, mean, mode, and HDI plotting.
    #   colour_density
    #       Colour for the density estimate.
    #   show_hdi_proportion_excluding_test_value:
    #       Show the (largest) HDI proportion excluding test_value.
    #   show_...
    #       Show quantiles, mean, mode, hdi
    #   ypos_...
    #       Ordinate (y) value at which to show quantiles, mean, mode.

    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    q <- quantile(sampled_values, probs = quantile_probs)
    my_mean <- mean(sampled_values)
    my_mode <- calculate_mode(sampled_values)
    my_ecdf <- ecdf(sampled_values)
    hdi_percent <- hdi_proportion * 100
    # debug_quantity(sampled_values)

    hdi_limits <- stanfunc$hdi(sampled_values, hdi_proportion)

    cump <- my_ecdf(test_value)
    if (cump <= 0.5) {
        # test_value in lower tail
        central_proportion_excluding_test_value <- 1 - 2 * cump
    } else {
        # test_value in upper tail
        central_proportion_excluding_test_value <- 1 - 2 * (1 - cump)
    }

    cat("\nParameter:", parname, "\n")
    cat("Mean:", my_mean, "\n")
    cat("Mode:", my_mode, "\n")
    cat("Quantiles:\n")
    print(q)
    cat(
        "Central proportion excluding test value of ", test_value, ": ",
        central_proportion_excluding_test_value,
        " (", 100 * central_proportion_excluding_test_value, "%)\n",
        sep = ""
    )
    cat(hdi_percent, "% HDI:\n", sep="")
    print(hdi_limits)
    if (show_hdi_proportion_excluding_test_value) {
        hdip <- hdi_proportion_excluding_test_value(sampled_values, test_value)
        cat(
            "HDI proportion excluding test value of ", test_value, ": ",
            hdip,
            " (", 100 * hdip, "%)\n",
            sep = ""
        )
    }
    #cat("Empirical CDF plotted\n")
    #plot(my_ecdf)

    cat(
        "Plotting posterior distribution, with ", hdi_percent, "% HDI\n",
        sep = ""
    )
    plot(
        my_density$x,
        my_density$y,
        xlab = parname,
        ylab = "Density",
        main = paste0("Posterior distribution of ", parname),
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
                 prettyNum(q[i], digits = digits), col = colour_quantiles)
            text(q[i], ypos_quantiles_lower,
                 paste0(quantile_probs[i] * 100, "%"),
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
             prettyNum(my_mean, digits = digits), col = colour_mean)
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
             prettyNum(my_mode, digits = digits), col = colour_mode)
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
             paste0(hdi_percent, "% HDI"), col = colour_hdi)
        text(hdi_limits[1], ypos_hdi_nums,
             prettyNum(hdi_limits[1], digits = digits), col = colour_hdi)
        text(hdi_limits[2], ypos_hdi_nums,
             prettyNum(hdi_limits[2], digits = digits), col = colour_hdi)
    }
}


stanfunc$ggplot_density_function <- function(
        sampled_values,
        parname = deparse(substitute(sampled_values)),
        # test_value = 0,
        quantile_probs = c(0.025, 0.5, 0.975),
        hdi_proportion = stanfunc$DEFAULT_HDI_PROPORTION,
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
        # show_hdi_proportion_excluding_test_value = FALSE,
        show_quantiles = TRUE,
        show_mean = TRUE,
        show_mode = TRUE,
        show_hdi = TRUE,
        ypos_quantiles = 1.15,
        ypos_mean = 0.6,
        ypos_mode = 0.4,
        ypos_hdi_text = NULL,
        ypos_hdi_nums = NULL,
        xlim = NULL,
        theme = theme_bw(),
        hdi_text_vjust = 0.5,
        hdi_nums_vjust = 0.5)
{
    # As for stanfunc$plot_density_function(), but generating and returning a
    # ggplot plot object.
    #
    # Args:
    #   sampled_values
    #       The values.
    #   parname
    #       Decorative parameter name, for the plot.
    #   quantile_probs
    #       Quantiles to plot.
    #   hdi_proportion
    #       HDI to plot (e.g. 0.95 for a 95% HDI).
    #   digits
    #       Number of digits for number formatting.
    #   colour_...
    #       Colours for quantiles, mean, mode, and HDI plotting.
    #   lty_...
    #       Line types for quantiles, mean, mode, and HDI plotting.
    #   colour_density
    #       Colour for the density estimate.
    #   show_...
    #       Show quantiles, mean, mode, hdi
    #   ypos_...
    #       Ordinate (y) value at which to show quantiles, mean, mode, HDI
    #       text, HDI numbers.
    #   xlim
    #       Optional x limits.
    #   theme
    #       ggplot theme.
    #   hdi_text_vjust
    #   hdi_nums_vjust
    #       Vertical justification for HDI text/numbers (0.5 = centred).

    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    q <- quantile(sampled_values, probs = quantile_probs)
    my_mean <- mean(sampled_values)
    my_mode <- calculate_mode(sampled_values)
    # my_ecdf <- ecdf(sampled_values)
    hdi_percent <- hdi_proportion * 100

    hdi_limits <- stanfunc$hdi(sampled_values, hdi_proportion)

    df <- data.frame(x = my_density$x, y = my_density$y)
    p <- (
        ggplot(df, aes(x, y))
        + theme
        + geom_line(colour = colour_density)
        + xlab(parname)
        + ylab("Density")
        + ggtitle(paste0("Posterior distribution of ", parname))
        + ylim(0, max_density * 1.2)
    )
    if (!is.null(xlim)) {
        p <- p + ggplot2::xlim(xlim)
    }

    ypos_quantiles_upper <- max_density * ypos_quantiles
    ypos_quantiles_lower <- max_density * (ypos_quantiles - 0.05)
    # ypos_quantiles_linetop <- max_density * (ypos_quantiles - 0.10)
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
                           label = paste0(quantile_probs[i] * 100, "%"),
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
                       label = prettyNum(my_mode, digits = digits),
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
        if (is.null(ypos_hdi_text)) {
            ypos_hdi_text <- mean_density_at_hdi + max_density * -0.05
        }
        if (is.null(ypos_hdi_nums)) {
            ypos_hdi_nums <- mean_density_at_hdi + max_density * 0.05
        }
        hdidf <- data.frame(
            x = c(hdi_limits[1], hdi_limits[1], hdi_limits[2], hdi_limits[2]),
            y = c(ypos_baseline, density_at_hdi[1], density_at_hdi[2], ypos_baseline)
        )
        print(hdidf)
        p <- (p
            + geom_line(
                data = hdidf,
                # aes = aes(x = x, y = y),
                colour = colour_hdi,
                linetype = lty_hdi,
            )
            + annotate("text", x = mean(hdi_limits), y = ypos_hdi_text,
                       label = paste0(hdi_percent, "% HDI"),
                       colour = colour_hdi,
                       vjust = hdi_text_vjust)
            + annotate("text", x = hdi_limits[1], y = ypos_hdi_nums,
                       label = prettyNum(hdi_limits[1], digits = digits),
                       colour = colour_hdi,
                       vjust = hdi_nums_vjust)
            + annotate("text", x = hdi_limits[2], y = ypos_hdi_nums,
                       label = prettyNum(hdi_limits[2], digits = digits),
                       colour = colour_hdi,
                       vjust = hdi_nums_vjust)
        )
    }
    return(p)
}


stanfunc$test_specific_parameter_from_stanfit <- function(fit, parname, ...)
{
    # Produce an annotated density plot for a Stan parameter.
    #
    # Args:
    #   fit
    #       The Stan fit object.
    #   parname
    #       The (single) parameter to plot.
    #   ...
    #       Other arguments to stanfunc$plot_density_function().

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
    # quantile(slot(fit, "sim")$samples[[1]]$"group_mean_reinf_rate[1]", probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

    # the opposite of quantile is ecdf:
    # x = slot(fit, "sim")$samples[[1]]$"gmd_side_stickiness_max"
    # EF = ecdf(x)
    # EF(0)

    # NOTE, however, that this contains all runs
    sampled_values <- stanfunc$sampled_values_from_stanfit(fit, parname)
    stanfunc$plot_density_function(sampled_values, parname, ...)
}


stanfunc$plot_multiple_stanfit_parameters <- function(fit, parnames, ...)
{
    # Plot annotated distributions for MULTIPLE parameters from a Stan fit,
    # using stanfunc$test_specific_parameter_from_stanfit() and thus
    # stanfunc$plot_density_function().

    npar <- length(parnames)
    nside <- ceiling(sqrt(npar))
    par(mfrow = c(nside, nside))
    for (i in 1:npar) {
        stanfunc$test_specific_parameter_from_stanfit(fit, parnames[i], ...)
    }
}


stanfunc$plot_all_stanfit_parameters <- function(fit, ...)
{
    # Plots annotated distributions for ALL parameters from a Stan fit,
    # using stanfunc$test_specific_parameter_from_stanfit() and thus
    # stanfunc$plot_density_function().

    parnames <- stanfunc$get_all_parameters_from_stanfit(fit)
    stanfunc$plot_multiple_stanfit_parameters(fit, parnames, ...)
}


stanfunc$points_to_mm <- function(pts)
{
    # Converts printers' points to millimetres.
    pts * 0.352777778
}


stanfunc$plot_multiple_stanfit_parameters_vstack <- function(
        fit,
        params,
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
        reverse_sign = FALSE,
        show_hdi_proportion_excluding_comparison = FALSE,
        hdi_proportion_fontsize_points = 8,
        colour_hdi = TRUE)
{
    # Creates what Jonathan Kanen (2019) has named a "TIE Fighter" plot of Stan
    # fitted parameters, i.e. vertically stacked with HDIs like wings.
    #
    # Args:
    #   fit
    #       The Stan fit object.
    #   params
    #       The parameters to plot, as a list of the form:
    #           list(
    #               list(name = name1, desc = desc1),
    #               list(name = name2, desc = desc2)
    #               ...
    #           )
    #       ... the inner bit being a list because c() can't hold expressions
    #       properly.
    #   inner_hdi_proportion
    #       The HDI proportion for the "liberal" (inner) of two HDIs to plot.
    #   outer_hdi_proportion
    #       The HDI proportion for the "conservative" (outer) of two HDIs to
    #       plot.
    #   xlab
    #       The x-axis label.
    #   ylab
    #       The y-axis label.
    #   title
    #       The plot title.
    #   compare_to
    #       The "test value" to compare to (e.g. 0).
    #   theme
    #       The ggplot theme.
    #   reverse_sign
    #       Flip the sign of all dependent variables? (For example, if you
    #       calculated CONTROL - DRUG when you meant DRUG - CONTROL.)
    #   show_hdi_proportion_excluding_comparison
    #       Show the (estimated) largest HDI interval that excludes
    #       "compare_to"?
    #   hdi_proportion_fontsize_points
    #       Font size, if show_hdi_proportion_excluding_comparison is used.
    #   colour_hdi
    #       Make the HDIs colourful?
    #
    # Returns:
    #   A ggplot object.

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
        !is.na(compare_to) && show_hdi_proportion_excluding_comparison
    )

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
                d$hdiprop_excluding_comparison[i] <- paste0(
                    100 * hdi_proportion_excluding_test_value(sampled_values,
                                                              compare_to)
                    , "%"
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
        + scale_colour_manual(values = colourmap, guide = "none")
        + theme
        + ggtitle(title)
    )
    return(p)
}


stanfunc$plot_all_stanfit_parameters_vstack <- function(fit, ...)
{
    # Plot ALL parameters of a Stan fit, via
    # stanfunc$plot_multiple_stanfit_parameters_vstack().

    parnames <- stanfunc$get_all_parameters_from_stanfit(fit)
    stanfunc$plot_multiple_stanfit_parameters_vstack(fit, parnames, ...)
}


stanfunc$generate_par_with_indices <- function(pn, pd)
{
    # Generates all parameter names for a parameter that has indices in the
    # Stan fit object.
    #
    # Args:
    #   pn
    #       Parameter name without indices, e.g. "alpha".
    #   pd
    #       Vector (length: number of dimensions) whose contents is the size
    #       in each dimension.
    #
    # Returns:
    #   For example, stanfunc$generate_par_with_indices("x", c(2, 3)) gives
    #   c("x[1,1]", "x[2,1]", "x[1,2]", "x[2,2]", "x[1,3]", "x[2,3]").

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
        name <- paste0(pn, "[")
        for (c in 1:ncol(indices)) {
            if (c > 1) name <- paste0(name, ",")
            name <- paste0(name, indices[r, c])
        }
        name <- paste0(name, "]")
        parnames <- c(parnames, name)
    }
    #cat("parnames: \n"); print(parnames)
    return(parnames)
}


stanfunc$get_all_parameters_from_stanfit <- function(fit)
{
    # Generates all the parameter names for a Stan fit, including e.g.
    # "alpha[1], alpha[2], alpha[3]..." for an array parameter alpha.

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
    # Returns the mean of sampled values for a specific parameter in a Stan
    # fit.

    sampled_values <- stanfunc$sampled_values_from_stanfit(fit, parname)
    return(mean(sampled_values))
}


stanfunc$get_parameter_means_from_stanfit <- function(fit, parnames)
{
    # Returns the means of sampled values for multiple named parameters in a
    # Stan fit.

    return(plyr::aaply(parnames, 1, .fun = function(x) {
        stanfunc$get_parameter_mean_from_stanfit(fit, x)
    }))
}


stanfunc$ggplot_specific_parameter_from_stanfit <- function(fit, parname, ...)
{
    # Uses stanfunc$ggplot_density_function() to plot a specific named
    # parameter from a Stan fit.

    sampled_values <- stanfunc$sampled_values_from_stanfit(fit, parname)
    return(stanfunc$ggplot_density_function(sampled_values, parname, ...))
}


stanfunc$extract_all_means_from_stanfit <- function(fit)
{
    # Produce all posterior means from a Stan fit.

    parnames <- stanfunc$get_all_parameters_from_stanfit(fit)
    means <- stanfunc$get_parameter_means_from_stanfit(fit, parnames)
    names(means) <- parnames
    return(means)
}


stanfunc$scatterplot_params <- function(fit, parnames = NULL, parlabels = NULL,
                                        ignore_lp = TRUE, ...)
{
    # Produce scatterplots (via R base graphics) for pairs of parameters in a
    # Stan fit. (Unwise not to restrict the parameter names for large fits!)
    #
    # Args:
    #   fit
    #       The Stan fit object.
    #   parnames
    #       Parameter names to plot. If NULL, all will be used.
    #   parlabels
    #       If supplied, pretty names, to match "parnames".
    #   ignore_lp
    #       Ignore the special Stan "lp__" variable?

    # https://stackoverflow.com/questions/3735286/create-a-matrix-of-scatterplots-pairs-equivalent-in-ggplot2
    if (is.null(parnames)) {
        parnames <- slot(fit, "sim")$pars_oi  # all parameter names
        if (ignore_lp) {
            parnames <- parnames[!(parnames == "lp__")]
        }
    }
    if (is.null(parlabels)) {
        parlabels <- parnames
    }
    values <- rstan::extract(fit, parnames, permuted = TRUE)
    d <- data.table(matrix(unlist(values), ncol = length(values), byrow = FALSE))
    colnames(d) <- parnames
    pairs(d, parlabels, ...)  # doesn't return anything useful, though
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("stanfunc" %in% search()) detach("stanfunc")
attach(stanfunc)  # subsequent additions not found, so attach at the end
