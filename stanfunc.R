# stanfunc.R

# Note re data.table:
# ... trailing [] to prevent "doesn't print first time" bug:
# https://stackoverflow.com/questions/32988099/data-table-objects-not-printed-after-returned-from-function
# https://github.com/Rdatatable/data.table/blob/master/NEWS.md#bug-fixes-5


library(bridgesampling)
library(coda)
library(data.table)
library(ggplot2)
library(matrixStats)
library(parallel)
library(reshape)
library(rstan)
library(stringr)


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

stanfunc <- new.env()

stanfunc$DEFAULT_CHAINS <- 8
stanfunc$DEFAULT_ITER <- 2000
stanfunc$DEFAULT_INIT <- "0"  # the Stan default, "random", uses the range -2 to +2
stanfunc$DEFAULT_SEED <- 1234  # for consistency across runs


#==============================================================================
# Core functions for e.g. rstan 2.16.2:
#==============================================================================

stanfunc$load_or_run_stan <- function(
        data,
        fit_filename,
        model_name,
        file = NULL,  # Filename for Stan code
        model_code = "",  # Text of Stan code
        save_stancode_filename = NULL,
        save_data_filename = NULL,
        save_cpp_filename = NULL,
        save_code_filename = NULL,  # old name for save_cpp_filename
        forcerun = FALSE,
        chains = stanfunc$DEFAULT_CHAINS,
        iter = stanfunc$DEFAULT_ITER,
        init = stanfunc$DEFAULT_INIT,  # the default, "random", uses the range -2 to +2
        seed = stanfunc$DEFAULT_SEED,  # for consistency across runs
        cache_filetype = c("rds", "rda"),
        ...)
{
    # Other potential common parameters:
    #   control = list(
    #       adapt_delta = 0.99
    #           # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    #           # https://www.rdocumentation.org/packages/rstanarm/versions/2.14.1/topics/adapt_delta
    #   )

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
            warning(paste(
                "Stan is not set to use all available CPU cores; using ",
                n_cores_stan, " when ", n_cores_available,
                " are available; retry after issuing the command\n",
                "    options(mc.cores = parallel::detectCores())",
                sep = ""))
        }
        if (n_cores_stan <= 1) {
            warning("Running with a single CPU core; Stan may be slow")
        }

        cat(paste("--- Running Stan, model ", model_name,
                  ", starting at ", Sys.time(), "...\n", sep = ""))

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
    algorithm = NULL,  # for the rare occasions when you want "Fixed_param"
    ...)
{
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
        cat(paste("Running variational Bayes approximation to Stan model ",
                  model_name, ", starting at ", Sys.time(), "...\n", sep = ""))

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
    init = stanfunc$DEFAULT_INIT,  # the default, "random", uses the range -2 to +2
    seed = stanfunc$DEFAULT_SEED,  # for consistency across runs
    control = NULL,
    vb = FALSE,  # quick-and-dirty variational Bayes
    save_code = FALSE,  # unnecessary; both Stan and C++ code is extractable from the Stan fit
    FIT_SUFFIX = "_stanfit.rds",
    BRIDGE_SUFFIX = "_bridgesampling.rds",
    VBFIT_SUFFIX = "_stanvbfit.rds",
    CPP_SUFFIX = "_code.cpp",
    ...)  # additional parameters to rstan::stan
{
    if (is.null(file) == (model_code == "")) {
        stop("Specify either 'file' or 'model_code' (and not both).")
    }

    # Note that C++ code is extractable from the Stan fit.
    fit_filename <- file.path(fit_cache_dir,
                              paste(model_name, FIT_SUFFIX, sep = ""))
    bridge_filename <- file.path(fit_cache_dir,
                              paste(model_name, BRIDGE_SUFFIX, sep = ""))
    vbfit_filename <- file.path(fit_cache_dir,
                                paste(model_name, VBFIT_SUFFIX, sep = ""))
    if (save_code) {
        cpp_filename <- file.path(fit_cache_dir,
                                  paste(model_name, CPP_SUFFIX, sep = ""))
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

        cat(paste("Running variational Bayes approximation to Stan model ",
                  model_name, "...\n", sep = ""))
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

        cat(paste("Running Stan model ", model_name, "...\n", sep = ""))

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


stanfunc$compare_model_evidence <- function(bridgesample_list_list,
                                            priors = NULL,
                                            detail = FALSE,
                                            rhat_warning_threshold = 1.1)
{
    # CHECK THE OUTPUT AGAINST, e.g.:
    # bridgesampling::post_prob(b1, b2, b3, b4, b5, b6, model_names = paste("Model", 1:6))
    # ... verified.

    # bridgesample_list_list
    #   A list of lists. Each item is a list with names:
    #           name: the model name
    #           bridgesample: the output from the
    #                   bridgesampling::bridge_sampler() function (an item of
    #                   class bridge_list)
    #           stanfit (optional): a corresponding Stan fit
    #                   ... useful to show e.g. maximum R-hat summaries
    #   (R note: if x is a list, then if x *doesn't* have item y, x$y == NULL.)
    #
    # priors:
    #   optional, but can be a vector containing prior probabilities for
    #   each model
    #
    # detail:
    #   keep the details used for intermediate calculations
    #
    # rhat_warning_threshold:
    #   If this threshold for R-hat is exceeded, warnings are shown. A value
    #   of 1.2 is a typical threshold and 1.1 is a stringent criterion (Brooks
    #   and Gelman 1998, doi:10.1080/10618600.1998.10474787, p. 444).
    #
    # Note:
    # - "marginal likelihood" is the same as "evidence" (e.g. Kruschke 2011
    #   p57-58)

    # https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
    # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
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
                        max_rhat <- stanfunc$max_rhat(fit)
                    }
                    return(c(
                        i,  # index
                        item$name,  # model_name
                        item$bridgesample$logml,  # log_marginal_likelihood
                        max_rhat  # max_rhat
                    ))
                },
                FUN.VALUE = c("index" = NA_integer_,
                              "model_name" = NA_character_,
                              "log_marginal_likelihood" = NA_real_,
                              "max_rhat" = NA_real_),
                y = bridgesample_list_list
            )
        )
    )
    d[, index := as.numeric(index)]
    d[, log_marginal_likelihood := as.numeric(log_marginal_likelihood)]
    d[, rhat_warning := ifelse(
        is.na(max_rhat),
        NA_character_,
        ifelse(max_rhat > rhat_warning_threshold, "WARNING: HIGH R-HAT", "OK")
    )]

    d[, model_rank := frank(-log_marginal_likelihood,
                            ties.method = "min")]  # "sports method"
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


stanfunc$params_with_high_rhat <- function(fit, threshold = 1.1)
{
    s <- stanfunc$summary_data_table(fit)
    return(s[Rhat >= threshold])
}


stanfunc$max_rhat <- function(fit)
{
    s <- stanfunc$summary_data_table(fit)
    return(max(s$Rhat))
}


stanfunc$summary_by_par_regex <- function(fit, pars = NULL,
                                          par_regex = NULL, ...)
{
    if (is.null(pars)) {
        s <- stanfunc$summary_data_table(fit, ...)
    } else {
        s <- stanfunc$summary_data_table(fit, pars = pars, ...)
    }
    # Optionally, filter on a regex
    if (!is.null(par_regex)) {
        s <- s[grepl(par_regex, s$parameter), ]
        if (nrow(s) == 0) {
            stop(paste0("No parameters match regex: ", par_regex))
        }
    }
    return(s)
}


stanfunc$annotated_parameters <- function(
        fit,
        pars = NULL,
        ci = c(0.025, 0.975),  # for the "nonzero" column
        probs = c(0.025, 0.25, 0.50, 0.75, 0.975),
        par_regex = NULL,
        annotate = TRUE,
        nonzero_as_hdi = TRUE,
        hdi_proportion = 0.95,
        hdi_method = "kruschke_mcmc"
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
        probs <- c(probs, c(0.0005, 0.9995,  # p < 0.001 two-tailed
                0.005, 0.995,  # for "**", p < 0.01 two-tailed
                0.025, 0.975,  # for "*", p < 0.05 two-tailed
                0.05, 0.95))  # for ".", p < 0.1 two-tailed
    } else {
        probs <- initial_probs
    }
    probs <- sort(unique(probs))
    s <- stanfunc$summary_by_par_regex(fit, pars = pars, probs = probs,
                                       par_regex = par_regex)
    # Find nonzero parameters (credible interval excludes zero)

    get_colname <- function(prob) {
        return(paste(prob * 100, "%", sep = ""))
    }

    nonzero_at <- function(lower, upper) {
        lower_name <- get_colname(lower)
        upper_name <- get_colname(upper)
        return(
            0 < s[, lower_name, with = FALSE] |
            s[, upper_name, with = FALSE] < 0
        )
    }

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
        ]
    }

    if (nonzero_as_hdi) {
        s[, hdi_lower := NA_real_]
        s[, hdi_upper := NA_real_]
        for (rownum in 1:nrow(s)) {
            parname <- s[rownum, parameter]
            values <- stanfunc$sampled_values_from_stanfit(fit, parname)
            hdi_pair <- hdi(values,
                            hdi_proportion = hdi_proportion,
                            method = hdi_method)
            s[rownum, hdi_lower := hdi_pair[1]]
            s[rownum, hdi_upper := hdi_pair[2]]
        }
        s[, nonzero := (0 < hdi_lower | hdi_upper < 0)]
    } else {
        s[, nonzero := nonzero_at(ci[1], ci[2])]
    }

    hidden_probs <- setdiff(probs, initial_probs)  # in former, not latter
    for (remove_prob in hidden_probs) {
        remove_colname <- get_colname(remove_prob)
        # cat(paste("Removing column", remove_colname, "\n"))
        s[, (remove_colname) := NULL]
    }

    s <- s[]  # fix nonprinting bug; see above
    return(s)
}


stanfunc$nonzero_parameters <- function(fit, annotate = FALSE, ...)
{
    s <- stanfunc$annotated_parameters(fit = fit, annotate = annotate, ...)
    s <- s[nonzero == TRUE][]  # restrict
    return(s)
}


stanfunc$code_from_stanfit <- function(fit)
{
    # Returns the Stan source code
    return(fit@stanmodel@model_code)
}


stanfunc$cpp_from_stanfit <- function(fit)
{
    # Returns the C++ code
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
    warning("stanfunc$parallel_stan: DEPRECATED; superseded by developments to rstan")
    cat("parallel_stan: cores = ", cores,
        ", chains = ", chains,
        ", iter = ", iter,
        ", seed = ", seed,
        "\n",
        sep = "")

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


stanfunc$HDIofMCMC <- function(sampleVec, credMass = 0.95)
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


stanfunc$TEST_HDI_OF_MCMC <- '
    # See Kruschke (2011) p41, p628
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


stanfunc$hdi_via_coda <- function(sampled_values, hdi_proportion = 0.95)
{
    hdi_limits_matrix <- coda::HPDinterval(as.mcmc(sampled_values),
                                           prob = hdi_proportion)
    # ... Sometimes crashes with
    # "Error in dimnames(x)[[2]] : subscript out of bounds"
    # for perfectly valid-looking data that works with other methods.

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
    cat("Bååth:\n")
    print(calculate_hdi_from_sample_piecewise(sampled_values, hdi_proportion))
    cat("Kruschke:\n")
    print(HDIofMCMC(sampled_values, hdi_proportion))
    cat("coda:\n")
    print(hdi_via_coda(sampled_values, hdi_proportion))
    cat("lme4:\n")
    print(hdi_via_lme4(sampled_values, hdi_proportion))
}


stanfunc$hdi <- function(sampled_values, hdi_proportion = 0.95,
                         method = c("kruschke_mcmc",
                                    "coda",
                                    "baath",
                                    "lme4"))
{
    # Method chooser!
    method <- match.arg(method)
    if (method == "kruschke_mcmc") {
        return(HDIofMCMC(sampled_values, hdi_proportion))
    } else if (method == "coda") {
        return(hdi_via_coda(sampled_values, hdi_proportion))
    } else if (method == "baath") {
        return(calculate_hdi_from_sample_piecewise(sampled_values, hdi_proportion))
    } else if (method == "lme4") {
        return(hdi_via_lme4(sampled_values, hdi_proportion))
    } else {
        stop("Bad method")
    }
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
                       "\n", sep = "")
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
    q <- quantile(sampled_values, probs = quantile_probs)
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
        main = paste("Posterior distribution of ", parname, sep = ""),
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
                 paste(quantile_probs[i] * 100, "%", sep = ""),
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
             paste(hdi_percent, "% HDI", sep = ""), col = colour_hdi)
        text(hdi_limits[1], ypos_hdi_nums,
             prettyNum(hdi_limits[1], digits = digits), col = colour_hdi)
        text(hdi_limits[2], ypos_hdi_nums,
             prettyNum(hdi_limits[2], digits = digits), col = colour_hdi)
    }
}


stanfunc$ggplot_density_function <- function(
        sampled_values,
        parname,
        test_value = 0,
        quantile_probs = c(0.025, 0.5, 0.975),
        hdi_proportion = 0.95,
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
        xlim = NULL,
        theme = theme_bw(),
        ypos_hdi_text = NULL,
        ypos_hdi_nums = NULL,
        hdi_text_vjust = 0.5,
        hdi_nums_vjust = 0.5)
{
    my_density <- density(sampled_values)
    max_density <- max(my_density$y)
    q <- quantile(sampled_values, probs = quantile_probs)
    my_mean <- mean(sampled_values)
    my_mode <- calculate_mode(sampled_values)
    my_ecdf <- ecdf(sampled_values)
    hdi_percent <- hdi_proportion * 100

    hdi_limits <- hdi(sampled_values, hdi_proportion)

    central_proportion_excluding_test_value <- 1 - 2 * my_ecdf(test_value)

    df <- data.frame(x = my_density$x, y = my_density$y)
    p <- (
        ggplot(df, aes(x, y))
        + theme
        + geom_line(colour = colour_density)
        + xlab(parname)
        + ylab("Density")
        + ggtitle(paste("Posterior distribution of ", parname, sep = ""))
        + ylim(0, max_density * 1.2)
    )
    if (!is.null(xlim)) {
        p <- p + ggplot2::xlim(xlim)
    }

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
                           label = paste(quantile_probs[i] * 100, "%", sep = ""),
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
                       label = paste(hdi_percent, "% HDI", sep = ""),
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
        params,  # list(list(name = name1, desc = desc1), list(name = name2, desc = desc2)...)
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
        name <- paste(pn, "[", sep = "")
        for (c in 1:ncol(indices)) {
            if (c > 1) name <- paste(name, ",", sep = "")
            name <- paste(name, indices[r,c], sep = "")
        }
        name <- paste(name, "]", sep = "")
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
    # quantile(slot(fit, "sim")$samples[[1]]$"group_mean_reinf_rate[1]", probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

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


stanfunc$scatterplot_params <- function(fit, parnames = NULL, parlabels = NULL,
                                        ignore_lp = TRUE, ...)
{
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


stanfunc$summarize_fit <- function(fit, par_regex = NULL, vb = FALSE)
{
    s <- stanfunc$summary_by_par_regex(fit, par_regex = par_regex)
    f <- function(x) {
        y <- sprintf("%.3f", x)
        y <- gsub("-", "–", y)
        return(y)
    }
    if (vb) {
        # Variational Bayes approximation; no R-hat
        maketextcol <- function(m, a, b) {
            paste0(f(m), " [", f(a), ", ", f(b), "]")
        }
        s[, summary := maketextcol(mean, s[["2.5%"]], s[["97.5%"]])]
    } else{
        # Full Stan fit
        maketextcol <- function(m, a, b, r) {
            paste0(f(m), " [", f(a), ", ", f(b), "] (R=", f(r), ")")
        }
        s[, summary := maketextcol(mean, s[["2.5%"]], s[["97.5%"]], s[["Rhat"]])]
    }
    s <- s[]  # for data table display bug
    return(s)
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("stanfunc" %in% search()) detach("stanfunc")
attach(stanfunc)  # subsequent additions not found, so attach at the end
