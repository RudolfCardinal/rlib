# miscsurv.R
#
# Miscellaneous functions for survival analysis.

# =============================================================================
# Packages
# =============================================================================

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        car,  # for car::Anova
        data.table,
        flextable,
        ftExtra,  # for markup within flextable tables
        rcompanion,  # for wilcoxonZ
        rlang,  # for dots_n
        tidyverse
    )
})

# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

miscsurv <- new.env()


# =============================================================================
# mk_survfit_stratum_table
# =============================================================================

miscsurv$mk_survfit_stratum_table <- function(survfit_object) {
    # Takes a survfit object, from survival::survfit() or
    # survminer::surv_fit().
    #
    # Creates a table with columns (see ?survfit.object):
    #       stratum
    #       n (in that stratum)
    #       time (the time points, t, at which the curve has a step)
    #       n.risk (number at risk at time t)
    #       n.censor (number exiting the risk set without an event at time t)
    #       surv (estimated proportion surviving at time t+0)
    #       cumhaz (cumulative hazard for each transition = -log(surv))
    #       std.err (standard error of the cumulative hazard) [2]
    #       upper (lower confidence interval for the survival curve) [1]
    #       lower (upper confidence interval for the survival curve) [1]
    #       conf.int (the level of the confidence intervals) [1]
    #
    # Notes:
    #
    # [1] By default the 95% confidence interval, but this is set by the
    # "conf.int" parameter to the function that created the survfit object.
    # Confidence intervals are calculated from standard error via the
    # "survfit_confint" function in
    # https://github.com/therneau/survival/blob/master/R/survfit.R.
    #
    # [2] For Cox proportional hazards, the calculation of std.error and
    # confidence intervals is via coxsurv.fit() in
    # https://github.com/therneau/survival/blob/master/R/coxsurvfit.R, and
    # survfit.coxph() in
    # https://github.com/therneau/survival/blob/master/R/survfit.coxph.R.
    #
    # For a demo "basic" survival object:
    #       library(survival)
    #       fit1 <- survfit(Surv(time, status) ~ x, data = aml)
    #       p1 <- survminer::ggsurvplot(fit1, conf.int = TRUE)
    #
    # A comparison with this function:
    #       library(patchwork)
    #       stratumtable <- miscsurv$mk_survfit_stratum_table(fit1)
    #       p2 <- ggplot(stratumtable, aes(x = time, y = surv, colour = stratum, fill = stratum)) + geom_step(aes(y = lower), linetype = "dotted") + geom_step(aes(y = upper), linetype = "dotted") + geom_step() + geom_point()
    #       # ... note: there isn't a simple geom_step() equivalent of geom_ribbon() yet.
    #       print(p1$plot | p2)
    #
    # For a demo via survminer::surv_fit():
    #       library(survfit)
    #       fit2 <- surv_fit(Surv(time, status) ~ x, data = aml)
    #
    # A version where confidence intervals are NOT 95%:
    #       fit3 <- survfit(Surv(time, status) ~ x, data = aml, conf.int = 0.8)

    sf <- survfit_object  # shorter name internally
    n_strata <- length(sf$strata)
    strata_names <- names(sf$strata)
    strata_lengths <- sf$strata
    d <- NULL
    cum_n <- 0
    for (i in 1:n_strata) {
        start_idx <- cum_n + 1
        end_idx <- cum_n + strata_lengths[i]
        d <- rbind(d, data.table(
            stratum = strata_names[i],
            # Order as in ?survfit.object:
            n = sf$n[i],
            time = sf$time[start_idx : end_idx],
            n.risk = sf$n.risk[start_idx : end_idx],
            n.enter = sf$n.enter[start_idx : end_idx],
            n.censor = sf$n.censor[start_idx : end_idx],
            surv = sf$surv[start_idx : end_idx],
            std.err = sf$std.err[start_idx : end_idx],
            cumhaz = sf$cumhaz[start_idx : end_idx],
            upper = sf$upper[start_idx : end_idx],
            lower = sf$lower[start_idx : end_idx],
            conf.int = sf$conf.int
        ))
        cum_n <- cum_n + strata_lengths[i]
    }
    return(d)
}


# =============================================================================
# Event time/survival calculations: piecewise survival
# =============================================================================

miscsurv$mk_piecewise_survival_table <- function(
    data,
    subject_id_col,
    dob_col,
    start_date_col,
    end_date_col,
    terminal_event_date_col,
    static_predictor_cols = NULL,
    latch_on_predictor_cols = NULL,
    latch_suffix_hx = "_hx",
    latch_suffix_cumtime = "_cumtime",
    time_units = "years",
    extra_slice_date_cols = NULL,
    additional_slice_dates = NULL
) {
    # Create a table for survival analysis by slicing each subject's timeline
    # up based on multiple predictors that change over time (in a "latch"
    # sense, i.e. once they come they are considered present subsequently),
    # together with time-invariant (static) predictors. For comparing the risk
    # of developing an event where exposure varies over time, and/or there are
    # covariates. See stats_demos/test_survival_methods.R, and especially
    # [Carstensen2023].
    #
    # Arguments:
    #   data
    #       A table of data, with one row per subject, and other relevant
    #       information in date columns.
    #   subject_id_col
    #       Name of a column (in "data") containing subject IDs of some sort.
    #   dob_col
    #       Name of a column (in "data") containing date of birth, for age
    #       calculations.
    #   start_date_col
    #       Name of a column (in "data") containing dates at which each
    #       subject's observation begins. Must not be blank.
    #   end_date_col
    #       Name of a column (in "data") containin dates at which each
    #       subject's observation ends (unless the outcome occurs -- the
    #       outcome will truncate observation earlier). Must not be blank.
    #   terminal_event_date_col
    #       Name of a column (in "data") containing the date at which the event
    #       of interest occurred, or blank (NA) if it did not occur. If this
    #       event occurs, it terminates the overall observation of the subject.
    #       In the output, this is reflected by a binary (numeric, 0/1) column
    #       indicating whether the event occurs in a given time slice.
    #       Optionally, the column can be renamed in the output by passing a
    #       named vector of length 1, e.g. c("event_occurred" = "event_date"),
    #       where the name is the output column name and the value is the input
    #       column name.
    #   static_predictor_cols
    #       Vector of column names (in "data") containing predictors that are
    #       static (fixed, temporally invariant) for each subject. Column type
    #       could be anything. Use NULL if there aren't any such columns.
    #   latch_on_predictor_cols
    #       Vector of column names (in "data") containing dates (or NA values),
    #       representing predictors that are assumed to start "off", and then
    #       if their date (of onset) is not NA, switch "on" at that date, and
    #       remain on subsequently. Optionally, the vector NAMES can be
    #       specified, e.g. c("had_stroke" = "stroke_date"), and these names
    #       will be used for the destination columns (in this example,
    #       "had_stroke" would be in the final output). (If the destination
    #       names are not specified, the names of the date columns will be
    #       used.) Use NULL if there aren't any such columns.
    #   latch_suffix_hx
    #       Latch predictors yield >1 column each. This suffix, for "history"
    #       (hx), is appended to create columns indicating "occurred in (at the
    #       start of) this or a preceding time interval" (1) or not (0).
    #   latch_suffix_cumtime
    #       Similarly, this suffix is used to indicate cumulative time since
    #       the onset of the predictor, to the end of the current interval.
    #   time_units
    #       The base unit to be used for time, when converting from dates to
    #       time (e.g. "years").
    #   extra_slice_date_cols
    #       Optional: column name(s) in "data", of columns containing lists of
    #       dates for the subject (within a column, one list per row). The
    #       resulting data will be "sliced" at each of these dates. List
    #       columns are supported by tibble() and data.table() objects, but not
    #       by data.frame().
    #   additional_slice_dates
    #       Optional: additional vector of dates at which to slice.
    #
    # Returns:
    #   A table containing one row per time interval (multiple rows per
    #   subject). These intervals are defined by times of interest, which
    #   include the subject's start date (from {{ start_date_col }}), the
    #   subject's end date (from {{ end_date_col }}), and any relevant times in
    #   between (from {{ latch_on_predictor_cols }}).
    #
    #   Columns:
    #       {{ subject_id_col }}            } named as in the original
    #       {{ static_predictor_cols }}     }
    #       t
    #           Time, in time_units (i.e. as a pure number), from the subject's
    #           start_date to the beginning of the relevant time interval. (The
    #           subject's start_date is thus implicitly coded as t = 0.) An
    #           INCLUSIVE time for this interval (see t_end).
    #       t_start
    #           Synonym for t.
    #       t_mid
    #           Middle of this time interval, in time units.
    #       t_end
    #           End of this time interval, in time units (= t + d). An
    #           EXCLUSIVE time, i.e. this interval is [t_start, t_end), or
    #           t_start <= some_time < t_end.
    #       d
    #           Duration of this time interval, in time_units, as a pure
    #           number. (Equal to t_end - t_start.)
    #       duration
    #           Synonym for d
    #       age_start
    #           Age, in time units, at t_start.
    #       age_mid
    #           Age, in time units, at the midpoint of t_start and t_end.
    #       age_end
    #           Age, in time units, at t_end.
    #       {{latch_on_predictor_cols}}_{{latch_suffix_hx}}
    #           "Latch" predictor output columns (optionally renamed, as
    #           above), with a suffix according to "latch_suffix_hx",
    #           indicating whether the event has occurred during (at the start
    #           of) or prior to this time interval.
    #       {{latch_on_predictor_cols}}_{{latch_suffix_cumtime}}
    #           Similarly, but for cumulative time since the onset of this
    #           latch predictor.
    #       {{ terminal_event_date_col }}
    #           The terminal event binary (0/1) column (optionally renamed, as
    #           above).
    #
    # NOT IMPLEMENTED, BUT CONSIDER:
    #
    # - Pulse variables, with specified pulse duration: akin to latch
    #   variables, but using the same concepts as the pulsetable functions
    #   above. Would need to think about the input data structure, since that
    #   needs >1 row per subject.

    # -------------------------------------------------------------------------
    # Local constants
    # -------------------------------------------------------------------------

    # -------------------------------------------------------------------------
    # Ancillary helper functions
    # -------------------------------------------------------------------------
    mktimediff <- function(d1, d2) {
        # Returns time (d2 - d1) in the units requested.
        # Works vectorized if required.
        datetimefunc$duration_units(d1, d2, units = time_units)
    }

    # -------------------------------------------------------------------------
    # Establish some final column names, and check arguments
    # -------------------------------------------------------------------------
    # Establish destination column names for latch-predictor columns:
    n_latch_cols <- length(latch_on_predictor_cols)
    latchnames <- names(latch_on_predictor_cols)
    # ... NULL if none are specified, e.g. try "names(c(1,2))"
    # ... a vector containing elements of "" if some are unspecified, e.g.
    #     try "names(c(1,b=2))"
    latch_final_col_names <- ifelse(
        is.null(latchnames) | latchnames == "",
        latch_on_predictor_cols,
        latchnames
    )
    rm(latchnames)
    names(latch_on_predictor_cols) <- NULL
    # ... otherwise our select() for "relevant" will also RENAME and confuse us

    stopifnot(length(terminal_event_date_col) == 1)
    ntedc <- names(terminal_event_date_col)
    terminal_event_output_col <- ifelse(
        is.null(ntedc),
        terminal_event_date_col,
        ntedc[1]
    )
    rm(ntedc)
    names(terminal_event_date_col) <- NULL

    stopifnot(
        latch_suffix_hx != ""
        && latch_suffix_cumtime != ""
        && latch_suffix_hx != latch_suffix_cumtime
    )

    n_extra_slice_date_cols <- length(extra_slice_date_cols)

    # -------------------------------------------------------------------------
    # Column name checks -- in the outer function, for speed
    # -------------------------------------------------------------------------
    # Establish column names not to conflict with (see splitter_fn):
    intermediate_extra_colnames <- c(
        "period_start_date", "period_end_date"
    )
    final_extra_colnames <- c(
        "t", "t_start", "t_mid", "t_end",
        "d", "duration",
        "age_start", "age_mid", "age_end"
    )
    extra_colnames <- c(intermediate_extra_colnames, final_extra_colnames)
    # Now check outcome and latch-predictor columns:
    stopifnot(!(terminal_event_output_col %in% extra_colnames))
    for (latch_col_name in latch_final_col_names) {
        stopifnot(!(latch_col_name %in% extra_colnames))
    }
    # Establish final fields, including for column sort order:
    latch_suffixes <- c(latch_suffix_hx, latch_suffix_cumtime)
    final_colnames <- final_extra_colnames
    if (n_latch_cols > 0) {
        for (i in 1:n_latch_cols) {
            final_colnames <- c(
                final_colnames,
                paste0(latch_final_col_names[i], latch_suffixes)
            )
        }
    }
    rm(latch_suffixes)
    final_colnames <- c(final_colnames, terminal_event_output_col)

    # -------------------------------------------------------------------------
    # Produce a set of intervals for one subject.
    # -------------------------------------------------------------------------
    splitter_fn <- function(x_data, y_key) {
        # cat("-- splitter_fn\n")
        # cat("... y_key:\n"); print(y_key)
        # cat("... x_data:\n"); print(x_data)
        # Basic checks
        stopifnot(nrow(x_data) == 1 && nrow(y_key) == 1)

        # Subject dates
        dob <- x_data %>% pull(dob_col)
        subjectstartdate <- x_data %>% pull(start_date_col)
        subjectenddate <- x_data %>% pull(end_date_col)
        eventdate <- x_data %>% pull(terminal_event_date_col)
        # The event terminates observation for the subject.
        # But if eventdate is NA, ignore it.
        subjectenddate <- min(eventdate, subjectenddate, na.rm = TRUE)
        stopifnot(subjectstartdate < subjectenddate)

        # Now, we create (potentially) multiple rows, each representing a time
        # interval. We start by determining dates of relevance: the start/end
        # dates, and the dates of any event of interest in between.
        relevant_dates <- c(
            subjectstartdate,
            subjectenddate,
            additional_slice_dates
        )
        if (n_latch_cols > 0) {
            relevant_dates <- c(
                relevant_dates,
                (x_data %>% select(all_of(latch_on_predictor_cols)))
            )
        }
        if (n_extra_slice_date_cols > 0) {
            extra_dates <- (
                x_data
                %>% select(all_of(extra_slice_date_cols))
                # ... gives a tibble of dimensions 1 (since x_data has one
                # row) x n_extra_slice_date_cols.
                %>% unnest(cols = all_of(extra_slice_date_cols))
                # ... gives a tibble of dimensions n_extra_slice_date_cols
                # tibble x (max list length); blanks have NULL.
                %>% unlist()
                # ... converts all non-NULL elements into a single vector
                # ... but also converts dates to numbers
                # ... and sometimes leaves NA values anyway
                %>% as.Date()
                # ... back to date.
            )
            # NA values will be filtered out in the next step anyway.
            relevant_dates <- c(relevant_dates, extra_dates)
        }
        relevant_dates <- relevant_dates[
            !is.na(relevant_dates)
            & relevant_dates >= subjectstartdate
            & relevant_dates <= subjectenddate
        ]
        relevant_dates <- sort(unique(relevant_dates))
        # - sort(unique()) is faster than unique(sort());
        #   https://stackoverflow.com/questions/36953026
        n_dates <- length(relevant_dates)

        # Those dates then define the intervals for our subject.
        # There must be at least two, since subjectstartdate < subjectenddate.
        subject_result <- tibble(
            interval_start_date = relevant_dates[1 : (n_dates - 1)],
            interval_end_date = relevant_dates[2: n_dates]
        )

        # Now we add some additional predictors:
        if (n_latch_cols > 0) {
            for (latchnum in 1:n_latch_cols) {
                src_latch_col <- latch_on_predictor_cols[latchnum]
                dst_latch_col <- latch_final_col_names[latchnum]
                predictor_onset_date <- x_data %>% pull(src_latch_col)
                subject_result <- (
                    subject_result
                    %>% mutate(
                        "{dst_latch_col}{latch_suffix_hx}" := as.numeric(
                            !is.na(predictor_onset_date)
                            & predictor_onset_date <= interval_start_date
                            # Once an interval goes to/past the latch predictor
                            # date, that predictor is latched ON.
                        ),
                        "{dst_latch_col}{latch_suffix_cumtime}" := (
                            mktimediff(predictor_onset_date, interval_end_date)
                            %>% replace_na(0)
                        )
                    )
                )
            }
        }

        # Tidy up and return results for this subject
        # - Possible that there is a speed advantage to doing the trivial
        #   calculations in the outer loop. But also nice to have these columns
        #   first. Not fully performance-tested.
        subject_result <- (
            subject_result
            %>% mutate(
                t_start = mktimediff(subjectstartdate, interval_start_date),
                duration = mktimediff(interval_start_date, interval_end_date),
                age_start = mktimediff(dob, interval_start_date),
                t = t_start,  # synonym
                d = duration,
                t_end = t_start + d,
                t_mid = t_start + d / 2,
                age_mid = age_start + d / 2,
                age_end = age_start + d,
                "{terminal_event_output_col}" := as.numeric(
                    !is.na(eventdate)
                    & interval_end_date == eventdate
                    # An interval may END with an event.
                )
            )
            %>% select(all_of(c(final_colnames)))  # Restrict/sort
        )
        return(subject_result)
    }

    # -------------------------------------------------------------------------
    # Produce results for all subjects, by grouping on subjects
    # -------------------------------------------------------------------------
    relevant_cols <- c(
        subject_id_col,
        dob_col,
        start_date_col,
        end_date_col,
        terminal_event_date_col,
        static_predictor_cols,
        latch_on_predictor_cols,
        extra_slice_date_cols
    )
    grouping_cols <- c(
        subject_id_col,
        static_predictor_cols
    )
    # - dplyr and referring to columns by name via variables:
    #   - https://dplyr.tidyverse.org/articles/programming.html
    #   - ?dplyr::select -- select() uses a <tidy-select> expression
    #   - ?dplyr::group_by -- group_by() doesn't
    relevant <- data %>% select(all_of(relevant_cols))
    # cat("... relevant:\n"); print(relevant)
    pieces <- (
        relevant
        %>% group_by(across(all_of(grouping_cols)))
        # Don't use summarize(); that produces one row per group.
        # Use group_modify here.
        %>% group_modify(~ splitter_fn(x_data = .x, y_key = .y))
        %>% ungroup()
    )
    # cat("... pieces:\n"); print(pieces)
    return(pieces)
}


miscsurv$test_piecewise_survival_tables <- function(verbose = TRUE) {
    # Test the creation of piecewise survival tables.
    d1 <- tibble(
        subject = c("Alice", "Bob", "Celia", "David", "Elizabeth", "Fred"),
        dob = as.Date(c(
            "2001-01-01", "2002-02-02", "2003-03-03", "2004-04-04",
            "2005-05-05", "2001-01-01"
        )),
        start_date = as.Date(c(
            "2011-01-01", "2012-02-02", "2013-03-03", "2014-04-04",
            "2015-05-05", "2020-01-01"
        )),
        end_date = as.Date(c(
            "2023-12-31", "2023-12-31", "2023-12-31", "2023-12-31",
            "2023-12-31", "2020-01-02"
        )),
        event_date = as.Date(c(
            "2019-06-06", NA, NA, NA, "2020-03-04", "2020-01-02"
        )),
        diabetic = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
        hypertensive = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
        stroke = as.Date(c(
            NA, NA, NA, NA, "2016-05-05", NA
        )),
        mi = as.Date(c(
            NA, NA, "2015-09-09", NA, "2019-03-03", NA
        )),
        extra_slice_dates_1 = list(
            NULL,
            as.Date(c("2020-01-01", "2021-01-01")),
            as.Date(c("2022-01-01", "2023-01-01")),
            NULL,
            NULL,
            NULL
        ),
        extra_slice_dates_2 = list(
            as.Date(c("2020-01-01", "2021-01-01")),
            NULL,
            NULL,
            as.Date(c("2022-01-01", "2023-01-01")),
            NULL,
            NULL
        )
    )
    if (verbose) {
        cat("- test_piecewise_survival_tables: source data d1:\n")
        print(d1)
    }

    # Standard
    x1 <- miscsurv$mk_piecewise_survival_table(
        data = d1,
        subject_id_col = "subject",
        dob_col = "dob",
        start_date_col = "start_date",
        end_date_col = "end_date",
        terminal_event_date_col = c("event_eg_died" = "event_date"),
        static_predictor_cols = c("diabetic", "hypertensive"),
        latch_on_predictor_cols = c(
            "cva" = "stroke",
            "mi"
        )
    )
    cat("\n- test_piecewise_survival_tables: result 1 (static + latch predictors):\n")
    print(x1)

    # No static predictors
    x2 <- miscsurv$mk_piecewise_survival_table(
        data = d1,
        subject_id_col = "subject",
        dob_col = "dob",
        start_date_col = "start_date",
        end_date_col = "end_date",
        terminal_event_date_col = c("event_eg_died" = "event_date"),
        static_predictor_cols = NULL,
        latch_on_predictor_cols = c(
            "cva" = "stroke",
            "mi"
        )
    )
    cat("\n- test_piecewise_survival_tables: result 2 (no static predictors):\n")
    print(x2)

    # Standard
    x3 <- miscsurv$mk_piecewise_survival_table(
        data = d1,
        subject_id_col = "subject",
        dob_col = "dob",
        start_date_col = "start_date",
        end_date_col = "end_date",
        terminal_event_date_col = c("event_eg_died" = "event_date"),
        static_predictor_cols = c("diabetic", "hypertensive"),
        latch_on_predictor_cols = NULL
    )
    cat("\n- test_piecewise_survival_tables: result 3 (no latch predictors):\n")
    print(x3)

    # No static or latch predictors
    x4 <- miscsurv$mk_piecewise_survival_table(
        data = d1,
        subject_id_col = "subject",
        dob_col = "dob",
        start_date_col = "start_date",
        end_date_col = "end_date",
        terminal_event_date_col = c("event_eg_died" = "event_date"),
        static_predictor_cols = NULL,
        latch_on_predictor_cols = NULL
    )
    cat("\n- test_piecewise_survival_tables: result 4 (no static or latch predictors):\n")
    print(x4)

    # Add extra slice dates:
    x5 <- miscsurv$mk_piecewise_survival_table(
        data = d1,
        subject_id_col = "subject",
        dob_col = "dob",
        start_date_col = "start_date",
        end_date_col = "end_date",
        terminal_event_date_col = c("event_eg_died" = "event_date"),
        static_predictor_cols = c("diabetic", "hypertensive"),
        latch_on_predictor_cols = c(
            "cva" = "stroke",
            "mi"
        ),
        # extra_slice_date_cols = c("extra_slice_dates_1")
        extra_slice_date_cols = c("extra_slice_dates_1", "extra_slice_dates_2"),
        additional_slice_dates = as.Date(c(
            "2015-01-01", "2016-01-01"
        ))
    )
    cat("\n- test_piecewise_survival_tables: result 5 (static + latch predictors + extra slice dates in two ways):\n")
    print(x5)


    # Now onto a more complex situation: time-varying binary predictors.
    # ***

    # *** also: add parallel processing via tidyverse futures
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscsurv" %in% search()) detach("miscsurv")
attach(miscsurv)  # subsequent additions not found, so attach at the end
