# datetimefunc.R

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_library <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) {
            if (!requireNamespace(p)) {
                install.packages(p)
            }
            library(p, character.only = TRUE)
        }
    }
    tmp_require_package_namespace(
        data.table,
        dplyr,
        ggplot2,
        lubridate,
        microbenchmark,
        patchwork
    )
    tmp_require_package_library(
        tidyverse
    )
})


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

datetimefunc <- new.env()


# =============================================================================
# Speed notes
# =============================================================================

if (FALSE) {
    # Speed of sorting two vectors by one of them:
    # This is a clear win for base_order.
    # See also https://stackoverflow.com/questions/72090199.
    v1 <- sample(50)  # random numbers from 1:50
    v2 <- 1:50
    microbenchmark::microbenchmark(
        data_table = {
            d <- data.table(v1 = v1, v2 = v2)
            data.table::setkeyv(d, c("v1", "v2"))
            list(
                v1 = d$v1,
                v2 = d$v2
            )
        },
        base_order = {
            ordering <- order(v1, v2)
            list(
                v1 = v1[ordering],
                v2 = v2[ordering]
            )
        },
        times = 1000
    )

    # ifelse() is *fractionally* faster than if_else().:
    v1 <- sample(1:1000);
    v2 <- sample(1:1000)
    microbenchmark(
        ifelse = if_else(v1 > v2, 1, 0),
        if_else = if_else(v1 > v2, 1, 0),
        times = 10000
    )
}


# =============================================================================
# Calculate age
# =============================================================================

datetimefunc$calendar_age_lubridate_1 <- function(dob, now) {
    # Works with POSIXct or Date values
    # http://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate
    interval_period <- lubridate::interval(dob, now)
    full_years <- interval_period %/% lubridate::years(1)
    # remaining_weeks <- (
    #     interval_period %% lubridate::years(1) %/% lubridate::weeks(1)
    # )
    # remaining_days <- (
    #     interval_period
    #     %% lubridate::years(1)
    #     %% lubridate::weeks(1)
    #     %/% lubridate::days(1)
    # )
    return(full_years)
}


datetimefunc$calendarAge <- datetimefunc$calendar_age_lubridate_1


datetimefunc$calendar_age <- function(dob, now)
{
    # THIS IS BETTER.
    # https://stackoverflow.com/questions/31126726
    # https://stackoverflow.com/questions/3611314/calculate-ages-in-r
    from_lt <- as.POSIXlt(dob)
    to_lt <- as.POSIXlt(now)

    age <- to_lt$year - from_lt$year

    return(ifelse(
        to_lt$mon < from_lt$mon |
            (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
        age - 1,
        age
    ))
}


datetimefunc$age_float_years <- function(dob, now, days_per_year = 365.25)
{
    days <- as.numeric(difftime(now, dob, units = "days"))
    return(days / days_per_year)
}


# =============================================================================
# Durations
# =============================================================================

datetimefunc$duration_units <- function(start_date, end_date, units) {
    # https://rawgit.com/rstudio/cheatsheets/main/lubridate.pdf
    # "Divide an interval by a duration to determine its physical length."
    (
        lubridate::interval(start = start_date, end = end_date) /
        lubridate::duration(1, units = units)
    )
}


datetimefunc$duration_years <- function(start_date, end_date) {
    datetimefunc$duration_units(start_date, end_date, units = "years")
}


datetimefunc$duration_days <- function(start_date, end_date) {
    datetimefunc$duration_units(start_date, end_date, units = "days")
}


# =============================================================================
# Event time calculations: pulsetable
# =============================================================================
# Functions relating to sequences of times representing "pulsed" events of a
# certain duration. These operate by creating an intermediate object that we'll
# call a "pulsetable" object (actually a list, one of whose elements is a
# table), which is then queried.
#
# Original purpose: renal function analysis, including e.g. "currently on
# lithium", "cumulative time on lithium", "time off lithium since starting".
#
# - Note that pmin(), pmax() mangle durations (e.g. values in days are treated
#   as seconds), so convert values to numeric first and back-convert
#   afterwards. Or, better, have dimensionless versions and then surround them
#   with date-specific versions.
#
# - Note also: base::ifelse() coerces everything to the same type.
#   dplyr::if_else() respects type better (but may complain about incompatible
#   types!).
#
# - Splitting time: one option is
#       indexes <- findInterval(time_now, start_times)
#   ... Defaults are correct; see ?findInterval and its first example:
#       rightmost.closed = FALSE
#       all.inside = FALSE
#       left.open = FALSE
#   If the "time_now" values are GREATER THAN OR EQUAL TO the test values, they
#   get assigned the index of the (greatest such) test value.
#
#   I'm not sure what's most elegant after that, though. The problem is that
#   the expression start_times[indexes] cannot be used, because indexes
#   contains 0 values (an invalid index) -- and the output vector in that
#   circumstance is of a DIFFERENT LENGTH and therefore garbled. Similar if you
#   use NA instead, etc. Core syntax is:
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Index-vectors
#   So can use dplyr::if_else(indexes == 0, NA, <something...>).
#
#   But we have moved to the "pulsetable" format, so this is less an issue.

# -----------------------------------------------------------------------------
# Create pulsetable objects
# -----------------------------------------------------------------------------

datetimefunc$mk_pulsetable_dimensionless <- function(
    event_times,
    event_durations,
    with_checks = FALSE
) {
    # Creates a "pulsetable" object, documenting events for a single subject,
    # with a presumed duration for each. This object can then be queried, e.g.
    # for dates, via query_pulsetable_ever(), query_pulsetable_times(), and
    # query_pulsetable_dates().
    #
    # This function DOES NOT MERGE events. But the query functions work
    # correctly regardless, and mk_intervaltable_from_pulsetable() can produce
    # a merged representation.
    #
    # This version of the function creates a dimensionless pulsetable object.
    # See also mk_pulsetable_dates().
    #
    # Arguments:
    #
    #   event_times
    #       Numeric times (e.g. ages in years) at which an event (e.g. lithium
    #       administration) occurred (more specifically, started to occur).
    #   event_durations
    #       Numeric time that each event is presumed to have endured for.
    #       Either a single number, or a vector as long as event_times.
    #       Consecutive events can "merge", functionally. This is not additive;
    #       for example, if event duration is 10 and events occur at times 20
    #       and 25, then this will be treated as an event starting at time 20
    #       and enduring until time 35.
    #   with_checks
    #       Validate inputs? Faster not to.
    #
    # Returns a list, with elements:
    #
    #   event_times
    #       As for the input (event_times), but sorted.
    #   event_durations
    #       As for the input, but sorted (to match event_times in the output).
    #   origin_date
    #       The date from which times are calculated, or NA if this object
    #       is not using dates. (From this function: always
    #       lubridate::NA_Date_. But included for compatibility with
    #       mk_pulsetable_dates().)
    #   time_units
    #       Units of time (e.g. "years" or NA). (From this function: always NA.
    #       But included for compatibility with mk_pulsetable_dates().)

    n_event_times <- length(event_times)
    n_durations <- length(event_durations)

    # Argument checks
    if (with_checks) {
        stopifnot(all(is.finite(event_times)))
        # ... excludes NA values (but 0-length OK)
        stopifnot(n_durations == 1 || n_durations == n_event_times)
        stopifnot(all(is.finite(event_durations)))
        stopifnot(all(event_durations > 0))
    }

    if (n_event_times == 0) {
        # Return a "non-event" result.
        return(list(
            event_times = NULL,  # NULL is equivalent to c()
            event_durations = NULL,
            origin_date = lubridate::NA_Date_,
            time_units = NA_character_
        ))
    }
    # From here on, we have at least one event.

    # Return the sorted values.
    if (n_durations > 1) {
        ordering <- order(event_times, event_durations)
        # ... sort by the first, then the second for tiebreaks
        ordered_durations <- event_durations[ordering]
    } else {
        # Single duration
        ordering <- order(event_times)
        ordered_durations <- event_durations
    }
    return(list(
        event_times = event_times[ordering],
        event_durations = ordered_durations,
        origin_date = lubridate::NA_Date_,
        time_units = NA_character_
    ))
}


datetimefunc$mk_pulsetable_dates <- function(
    origin_date,
    event_dates,
    event_durations,
    time_units = "years"
) {
    # As for mk_pulsetable_dimensionless(), which it relies on, but using
    # dates.
    #
    # Arguments:
    #
    #   origin_date
    #       A date before all others, used as the reference date. Typically the
    #       DOB for a patient, making "times" equivalent to "ages".
    #   event_dates
    #       Should be of type as.Date().
    #   event_durations
    #       Should be a lubridate duration in a sensible (constant) unit, e.g.
    #       lubridate::duration(30, units = "days") -- or a vector of these, of
    #       the same length as event_dates. Though lubridate does use
    #       constants, e.g. 365.25 days per year for "years" (see
    #       ?lubridate::duration), so "years" is fine.
    #   time_units
    #       Textual units (e.g. "days", "years") to operate with internally.
    #       As used by the lubridate package.
    #
    # Returns:
    #
    #   As for mk_pulsetable_dimensionless(), but this version of the
    #   pulsetable object will carry date "anchoring" information too.

    if (!(length(origin_date) == 1
            && lubridate::is.Date(origin_date))) {
        stop("Bad origin_date parameter: ", origin_date)
    }
    n_event_dates <- length(event_dates)
    if (!(n_event_dates == 0 || all(lubridate::is.Date(event_dates)))) {
        stop("Bad event_dates parameter: ", event_dates)
    }
    n_event_durations <- length(event_durations)
    if (!(
        (n_event_durations == 1 || n_event_durations == n_event_dates)
        && all(lubridate::is.duration(event_durations))
    )) {
        stop("Bad event_durations parameter: ", event_durations)
    }
    pt <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = datetimefunc$duration_units(
            start_date = origin_date,
            end_date = event_dates,
            units = time_units
        ),
        event_durations = (
            event_durations /
            lubridate::duration(1, units = time_units)
        )
    )
    pt$origin_date <- origin_date
    pt$time_units <- time_units
    return(pt)
}


# -----------------------------------------------------------------------------
# Make interval tables
# -----------------------------------------------------------------------------

datetimefunc$mk_intervaltable_from_pulsetable <- function(
    pt,
    merge_events = TRUE,
    include_non_event_intervals = TRUE
) {
    # From a "pulsetable" object (see e.g. mk_pulsetable_dimensionless()),
    # create an "interval table". This is mostly a cosmetic thing, for human
    # inspection!
    #
    # Arguments:
    #
    #   merge_events
    #       Merge consecutive contiguous events?
    #   include_non_event_intervals
    #       If FALSE, rows with "event == FALSE" are not included.
    #
    # Returns:
    #   A data.table, usually containing one time interval per row,
    #   collectively covering all time from -Inf to +Inf with no gaps. Columns
    #   are:
    #
    #       t_start
    #           Numeric time at which each interval starts.
    #       t_end
    #           Numeric time at which each interval ends. The interval is
    #           represented by [start, end), i.e. inclusive start, exclusive
    #           end.
    #       duration
    #           Interval duration.
    #       event
    #           Is the event occurring (TRUE) or not (FALSE) during this
    #           interval?
    #       event_duration
    #           Interval duration if the event is occurring, or 0 if it's not.
    #       cum_event_dur
    #           Cumulative duration, to the END of the interval, spent "during"
    #           events.
    #       t_start_date (D)
    #           Only present if the pulsetable object had date information.
    #           Date corresponding to the start of the interval.
    #       t_end_date (D)
    #           Only present if the pulsetable object had date information.
    #           Date corresponding to the end of the interval.

    n_events <- length(pt$event_times)

    # Deal with "no intervals".
    if (n_events == 0) {
        # Return a "non-event" result.
        if (include_non_event_intervals) {
            return(data.table(
                t_start = -Inf,
                t_end = Inf,
                event = FALSE,
                duration = Inf,
                event_duration = 0,
                cum_event_dur = 0
            ))
        } else {
            return(NULL)
        }
    }

    # Start with start time and duration.
    intervals <- data.table::data.table(
        t_start = pt$event_times,
        src_duration = pt$event_durations
    )
    # These are PRE-SORTED by the pulsetable object.

    # Intervals where an event occurred:
    # - Calculate end times.
    if (n_events == 1) {
        # Single event: simple calculation
        intervals[, t_end := t_start + src_duration]
        intervals[, src_duration := NULL]
    } else {
        # More than one event.
        intervals[
            ,
            t_end := pmin(
                # When this event expires:
                t_start + src_duration,
                # When the next event starts:
                c(t_start[2 : n_events], Inf)
            )
            # ... up until the expiry of this event, or until the next
            # event starts
        ]

        # Merge consecutive contiguous events, if required.
        # (Only applicable if more than one event.)
        if (merge_events) {
            # Already sorted by t_start then src_duration, as above.
            intervals[
                ,
                prev_event_ends := c(-Inf, t_end[1 : (n_events - 1)])
            ]
            intervals[
                ,
                groupnum := cumsum(
                    ifelse(t_start > prev_event_ends, 1, 0)
                    # ... 1 if we're starting a new group, 0 otherwise.
                )
            ]
            intervals <- intervals[
                ,
                .(
                    t_start = min(t_start),
                    t_end = max(t_end)
                ),
                by = groupnum
            ]
            # ... creates a table with columns groupnum, t_start, t_end (only)
            intervals[, groupnum := NULL]
        } else {
            intervals[, src_duration := NULL]
        }
    }
    intervals[, event := TRUE]

    if (include_non_event_intervals) {
        # Add in "non-occurring" events: the gaps.
        # Any zero-length intervals will be culled later.
        intervals_not_occurring <- data.table::data.table(
            t_start = c(-Inf, intervals$t_end),
            t_end = c(intervals$t_start, Inf),
            event = FALSE
        )
        intervals <- rbind(intervals, intervals_not_occurring)
    }

    # Finish off calculations.
    intervals[, duration := t_end - t_start]
    # Eliminate any dummy non-event intervals:
    intervals <- intervals[duration > 0]
    data.table::setkeyv(intervals, c("t_start", "duration"))
    intervals[, event_duration := ifelse(event, duration, 0)]
    # ... don't multiply; duration may be infinite
    intervals[, cum_event_dur := cumsum(event_duration)]

    # Add dates?
    if (!is.na(pt$origin_date)) {
        intervals[, t_start_date := as.Date(
            pt$origin_date + lubridate::duration(t_start, units = pt$time_units)
        )]
        intervals[, t_end_date := as.Date(
            pt$origin_date + lubridate::duration(t_end, units = pt$time_units)
        )]
        # A date plus "1 day" gives a date, but a date plus "1 year" gives a
        # datetime, so we'll force to a date.
    }

    return(intervals)
}


# -----------------------------------------------------------------------------
# Query pulsetable objects
# -----------------------------------------------------------------------------

datetimefunc$query_pulsetable_ever <- function(pt) {
    # Queries a pulsetable in a very basic way: "Did the event ever occur?"

    # Don't rely on the existence of pt$intervals; that is only non-NULL if
    # include_interval_table was set during pulsetable creation.
    return(length(pt$event_times) > 0)
}


datetimefunc$query_pulsetable_times_v1 <- function(pulsetable, query_times) {
    # Query a pulse table (see above -- relating to a single subject) at
    # various times (in the dimensionless time units used within the
    # pulsetable).
    #
    # This version is vectorized and is sufficiently fast. It uses only the
    # very plain data from mk_pulsetable_dimensionless(), i.e. the event_times
    # and event_durations; it doesn't use the interval table.
    #
    # Arguments:
    #
    #   pulsetable
    #       The pulse table object (list, as above) to query.
    #   query_times
    #       The (dimensionless) times at which to produce a row in the output.
    #
    # Returns a data.table with columns:
    #
    #   t
    #       The input times, i.e. query_times.
    #   current
    #       Is the event occurring at this time? (Boolean.)
    #   hx
    #       Has the event happened at/before time t? (Boolean.)
    #   t_since_first
    #       Time since the first occurrence (0 if before or has never
    #       occurred).
    #   cum_t_on
    #       Cumulative time that the subject has been exposed to the event, at
    #       time t.
    #   cum_t_after
    #       Cumulative time that the subject has had OFF exposure, after the
    #       first exposure. (Will be t_since_first - cum_t_on.)
    #   t_since_last
    #       Time since the last known exposure.
    #   ever
    #       Does the event ever occur for the subject (across the lifetime,
    #       including in the future)? Boolean. Obviously, this value will be
    #       the same for all values of t.

    stopifnot(all(is.numeric(query_times)))
    # ... prevents NA values; also fails for empty input
    event_times <- pulsetable$event_times  # sorted, but may be empty
    event_durations <- pulsetable$event_durations
    # ... a single number or a vector matching event_times
    n_events <- length(event_times)
    n_durations <- length(event_durations)

    ever <- n_events > 0

    if (ever) {
        # ---------------------------------------------------------------------
        # current
        # ---------------------------------------------------------------------
        # - https://stackoverflow.com/questions/60584096/how-to-check-if-pairs-from-df2-are-in-pairs-of-df1-inclusive-in-r
        # - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/findInterval
        # - findInterval() requires ascending (non-decreasing) values in its
        #   second argument.
        # - use dplyr::if_else() rather than base::ifelse() if types are a
        #   concern, because dplyr::if_else() preserves type.

        indexes_danger <- findInterval(query_times, event_times)
        # ... Defaults are correct; see ?findInterval and its first example:
        #   rightmost.closed = FALSE
        #   all.inside = FALSE
        #   left.open = FALSE
        # - If the "x" values are GREATER THAN OR EQUAL TO the test values
        #   ("vec"), they get assigned the index of the (greatest such) test
        #   value. If an "x" value is SMALLER THAN ALL the test values, the
        #   output is 0. (And that creates an indexing danger, as below.)
        # - The second argument must be non-decreasing and not contain NAs,
        #   but findInterval() checks that itself.

        # In what follows, beware the following. The expression
        # event_times[indexes_danger] cannot be used, because indexes_danger
        # contains 0 values (an invalid index) -- and the output vector in that
        # circumstance is of a DIFFERENT LENGTH and therefore garbled. Similar
        # if you use NA instead, etc. Core syntax is:
        # https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Index-vectors
        # Therefore, we must create indexes_safer, but only use its lookup
        # values when indexes_danger != 0.
        invalid_indexes <- indexes_danger == 0
        indexes_safer <- ifelse(invalid_indexes, 1, indexes_danger)
        relevant_starts <- event_times[indexes_safer]
        if (n_durations > 1) {
            relevant_durations <- event_durations[indexes_safer]
        } else {
            relevant_durations <- event_durations  # single number
        }

        current <- ifelse(
            invalid_indexes,  # query time before first start time?
            FALSE,  # if so, then definitely not current
            query_times < relevant_starts + relevant_durations
            # We use "up to (not including) the end time".
        )

        # ---------------------------------------------------------------------
        # hx (history)
        # ---------------------------------------------------------------------
        # Could use:
        #   first_event <- min(event_times)
        # but we can instead rely on mk_pulsetable_dimensionless() sorting its
        # event times, and use:
        first_event <- event_times[1]
        hx <- query_times >= first_event

        # ---------------------------------------------------------------------
        # t_since_first
        # ---------------------------------------------------------------------
        t_since_first <- pmax(0, query_times - first_event)

        # ---------------------------------------------------------------------
        # cum_t_on, cum_t_after, t_since_last
        # ---------------------------------------------------------------------
        if (n_events >= 2) {
            event_ends <- pmin(  # until the first of:
                event_times + event_durations,  # this event ends...
                c(event_times[2 : n_events], Inf)  # ... or the next one starts
            )
            event_durations <- event_ends - event_times
            cum_durations <- cumsum(event_durations)  # at the end of events
            cum_duration_at_start <- c(0, cum_durations[1 : n_events - 1])
            last_exposure <- max(event_ends)
        } else {
            # n_events is 1
            cum_duration_at_start <- 0
            last_exposure <- event_times + event_durations
        }
        cum_t_on <- ifelse(
            invalid_indexes,  # is the query time before the first start time?
            0,
            (
                # Cumulative time at the start of the latest relevant event
                cum_duration_at_start[indexes_safer]
                +
                # Applicable time since the start of the latest relevant event
                pmin(
                    query_times - relevant_starts,
                    relevant_durations
                )
            )
        )
        cum_t_after <- t_since_first - cum_t_on
        t_since_last <- pmax(0, query_times - last_exposure)

        # cat("- query_pulsetable_times():\n")
        # cat("... query_times:\n"); print(query_times)
        # cat("... event_times:\n"); print(event_times)
        # cat("... event_durations:\n"); print(event_durations)
        # cat("... indexes_danger:\n"); print(indexes_danger)
        # cat("... invalid_indexes:\n"); print(invalid_indexes)
        # cat("... indexes_safer:\n"); print(indexes_safer)
        # cat("... current:\n"); print(current)
        # cat("... event_durations:\n"); print(event_durations)
        # cat("... cum_durations:\n"); print(cum_durations)
        # cat("... cum_duration_at_start:\n"); print(cum_duration_at_start)
        # cat("... cum_t_on:\n"); print(cum_t_on)
        # cat("... t_since_last:\n"); print(t_since_last)

    } else {
        # never!
        current <- FALSE
        hx <- FALSE
        t_since_first <- 0
        cum_t_on <- 0
        cum_t_after <- 0
        t_since_last <- 0
    }

    return(data.table(
        t = query_times,
        current = current,
        hx = hx,
        t_since_first = t_since_first,
        cum_t_on = cum_t_on,
        cum_t_after = cum_t_after,
        t_since_last = t_since_last,
        ever = ever
    ))
}


datetimefunc$query_pulsetable_times_v2 <- function(pulsetable, query_times) {
    # See query_pulsetable_times() above. This version is not vectorized and is
    # TOO SLOW: e.g. 19.3 seconds for 1000 interations with p1/p1_test_times in
    # the test function. It uses the interval table.

    stopifnot(all(is.numeric(query_times)))  # also fails for empty input
    intervals <- datetimefunc$mk_intervaltable_from_pulsetable(pulsetable)
    ever <- any(intervals$event)
    last_exposure <- ifelse(
        ever,
        max(intervals %>% filter(event == TRUE) %>% pull(t_end)),
        NA_real_
    )
    query_fn <- function(t) {  # t is a SINGLE VALUE, the query time.
        # Slices
        previous_and_current_intervals <- (
            # "finishes before t [= previous], or starts before/at t and not
            # yet finished [= current]"
            intervals
            %>% filter(t_end <= t | (t_start <= t & t < t_end))
            # ... remembering that t_end is exclusive, not inclusive.
        )
        current_interval <- (
            previous_and_current_intervals
            %>% slice_tail(n = 1)
        )
        first_prev_current_with_event <- (
            previous_and_current_intervals
            %>% filter(event == TRUE)
            %>% slice_head(n = 1)
        )
        # Booleans
        hx <- nrow(first_prev_current_with_event) > 0
        current <- current_interval$event
        # Debugging
        # cat("--- t =", t, "\n")
        # cat("current_interval:\n"); print(current_interval)
        # cat("previous_and_current_intervals:\n"); print(previous_and_current_intervals)
        # cat("first_prev_current_with_event:\n"); print(first_prev_current_with_event)
        # Time calculations
        if (hx) {
            # t_since_first
            t_first <- first_prev_current_with_event$t_start
            t_since_first <- t - t_first
            # cum_t_on
            t_during_this_interval <- t - current_interval$t_start
            if (current) {
                t_on_during_this_interval <- t_during_this_interval
                t_off_during_this_interval <- 0
            } else {
                t_on_during_this_interval <- 0
                t_off_during_this_interval <- t_during_this_interval
            }
            previous_intervals <- (
                previous_and_current_intervals %>% slice_head(n = -1)
            )
            last_previous_interval <- previous_intervals %>% slice_tail(n = 1)
            cum_t_on <- t_on_during_this_interval
            if (nrow(last_previous_interval) > 0) {
                cum_t_on <- cum_t_on + last_previous_interval$cum_event_dur
            }
            # cum_t_after
            prev_off_intervals_after_first <- (
                previous_intervals
                %>% filter(t_first <= t_start & !event)
            )
            cum_t_after <- (
                sum(prev_off_intervals_after_first$duration)
                + t_off_during_this_interval
            )
            t_since_last <- max(0, t - last_exposure)
            # debugging:
            # cat("previous_intervals:\n"); print(previous_intervals)
            # cat("last_previous_interval:\n"); print(last_previous_interval)
            # cat("t_first:\n"); print(t_first)
            # cat("t_on_during_this_interval:\n"); print(t_on_during_this_interval)
            # cat("t_off_during_this_interval:\n"); print(t_off_during_this_interval)
            # cat("prev_off_intervals_after_first:\n"); print(prev_off_intervals_after_first)
        } else {
            t_since_first <- 0
            cum_t_on <- 0
            cum_t_after <- 0
            t_since_last <- 0
        }
        return(data.table(
            t = t,
            current = current,
            hx = hx,
            t_since_first = t_since_first,
            cum_t_on = cum_t_on,
            cum_t_after = cum_t_after,
            t_since_last = t_since_last
        ))
    }
    result <- (
        tibble(t = query_times)
        %>% rowwise()
        %>% reframe(query_fn(t))
        %>% mutate(ever = ever)
        %>% as.data.table()
    )
    return(result)
}


# Pick the fastest:
datetimefunc$query_pulsetable_times <- datetimefunc$query_pulsetable_times_v1


datetimefunc$query_pulsetable_dates <- function(
    pulsetable,
    query_dates,
    query_pulsetable_fn = datetimefunc$query_pulsetable_times
) {
    # Query a pulsetable using dates (not dimensionless times).
    #
    # Arguments:
    #
    #   pulsetable
    #       The pulse table to query (using dimensionless times).
    #   query_dates
    #       The dates to produce results for, analogous to query_times for
    #       query_pulsetable_times().
    #   query_pulsetable_fn
    #       The underlying function to use. Defaults to the fastest we have.
    #       Only varied for cross-checking during testing.
    #
    # Returns:
    #   As for query_pulsetable_times(), but now the column "t" is a date
    #   column, and there is an additional column:
    #
    #       t_raw
    #           The underlying "t" column.

    stopifnot(all(lubridate::is.Date(query_dates)))
    if (is.na(pulsetable$origin_date) || is.na(pulsetable$time_units)) {
        stop(paste0(
            "For query_pulsetable_dates(), use a pulsetable created by ",
            "mk_pulsetable_dates()"
        ))
    }

    query_times <- datetimefunc$duration_units(
        start_date = pulsetable$origin_date,
        end_date = query_dates,
        units = pulsetable$time_units
    )
    q <- query_pulsetable_fn(
        pulsetable = pulsetable,
        query_times = query_times
    )
    q[, t_raw := t]
    q[, t := query_dates]
    return(q)
}


# -----------------------------------------------------------------------------
# Test pulsetable functions
# -----------------------------------------------------------------------------

datetimefunc$ensure_two_tables_equal <- function(x, y) {
    # - use all.equal(), not identical(); the latter reports differences that
    #   are due to infinitesimal floating-point differences; the first doesn't
    #   care (appropriately).
    # - if discrepancy: print(q1a == q1b), then fix bug!
    #
    # HOWEVER, sometimes this fails:
    #   stopifnot(isTRUE(all.equal(e2a$end, e2c$end)))
    # ... This doesn't work: you get
    #   [1] "Attributes: < Names: 2 string mismatches >"                               "Attributes: < Length mismatch: comparison on first 2 components >"
    #   [3] "Attributes: < Component 1: Modes: character, externalptr >"               "Attributes: < Component 1: Lengths: 3, 1 >"
    #   [5] "Attributes: < Component 1: target is character, current is externalptr >" "Attributes: < Component 2: Modes: numeric, character >"
    #   [7] "Attributes: < Component 2: Lengths: 8, 3 >"                               "Attributes: < Component 2: target is numeric, current is character >"
    # Yet this is fine:
    #   stopifnot(isTRUE(all.equal(e2a$start, e2c$start)))
    #   stopifnot(isTRUE(all.equal(e2a$end, e2c$end)))

    xlab <- deparse(substitute(x))
    ylab <- deparse(substitute(y))
    xcols <- sort(colnames(x))
    ycols <- sort(colnames(y))
    if (!identical(xcols, ycols)) {
        cat("Sorted columns  for ", xlab, ":\n", sep = "")
        print(xcols)
        cat("Sorted columns  for ", ylab, ":\n", sep = "")
        print(ycols)
        stop(paste0(xlab, " and ", ylab, " have different column names"))
    }
    for (colname in xcols) {
        if (!isTRUE(all.equal(x[[colname]], y[[colname]]))) {
            print(x == y)
            stop(paste0(xlab, " and ", ylab, " differ in column ", colname))
        }
    }
    cat(paste0(xlab, " and ", ylab, " are functionally identical\n"))
}

datetimefunc$test_pulsetable <- function(verbose = TRUE) {
    # Create and query pulsetable objects.
    line <- paste(rep("=", 79), collapse = "")
    mktitle <- function(x) {
        cat(line, "\n- ", x, "\n", line, "\n", sep = "")
    }

    # -------------------------------------------------------------------------
    # 1
    # -------------------------------------------------------------------------

    p1a <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(5, 20, 100, 105, 150, 200),
        event_durations = 10
    )
    p1ai <- datetimefunc$mk_intervaltable_from_pulsetable(p1a)
    if (verbose) {
        mktitle("p1")
        print(p1a)
        print(p1ai)
    }

    # Very basic query:
    q1a_ever <- datetimefunc$query_pulsetable_ever(p1a)
    if (verbose) {
        mktitle("q1a_ever")
        print(q1a_ever)
    }

    p1b <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(5, 20, 100, 105, 150, 200),
        event_durations = 10
    )  # same as p1a
    q1b_ever <- datetimefunc$query_pulsetable_ever(p1b)
    stopifnot(identical(q1a_ever, q1b_ever))

    # Times, two ways:
    p1_test_times <- c(0, 5, 7, 17, 50, 103, 115, 500)
    q1a <- datetimefunc$query_pulsetable_times_v1(p1a, p1_test_times)
    q1b <- datetimefunc$query_pulsetable_times_v2(p1a, p1_test_times)
    if (verbose) {
        mktitle("q1a")
        print(q1a)
        mktitle("q1b")
        print(q1b)
    }
    datetimefunc$ensure_two_tables_equal(q1a, q1b)

    # Speed test
    n_tests <- 1000
    mktitle("Speed test, query_pulsetable_times")
    tmp_start <- Sys.time()
    for (i in 1:n_tests) {
        datetimefunc$query_pulsetable_times(p1a, p1_test_times)
    }
    tmp_end <- Sys.time()
    cat("- ", n_tests, " iterations took:\n", sep = "")
    print(tmp_end - tmp_start)
    # 19.3 seconds for query_pulsetable_times_slow (otter)
    # ~0.5 seconds for query_pulsetable_times (otter) [or ~1.3s on shrike]
    # ... though up to ~1.2 s when fiddling around with conditionality

    # cat("Speed test, query_pulsetable_times with few columns (hx only):\n")
    # tmp_start <- Sys.time()
    # for (i in 1:n_tests) {
    #     datetimefunc$query_pulsetable_times(
    #         p1,
    #         p1_test_times,
    #         with_current = FALSE,
    #         with_hx = TRUE,
    #         with_t_since_first = FALSE,
    #         with_cum_t_on = FALSE,
    #         with_cum_t_after = FALSE
    #     )
    # }
    # tmp_end <- Sys.time()
    # cat("- ", n_tests, " iterations took:\n", sep = "")
    # print(tmp_end - tmp_start)
    # # ... about 1.14 s (otter); so conditionality on variables NOT helpful.

    # -------------------------------------------------------------------------
    # 2
    # -------------------------------------------------------------------------

    p2_origin_date <- as.Date("1900-01-01")
    p2_time_units <- "years"
    p2 <- datetimefunc$mk_pulsetable_dates(
        origin_date = p2_origin_date,
        event_dates = as.Date(c(
            "1910-01-01", "1915-01-01", "1915-06-01", "1940-01-01"
        )),
        event_durations = lubridate::duration(300, units = "days"),
        time_units = p2_time_units
    )
    p2i <- datetimefunc$mk_intervaltable_from_pulsetable(p2)
    if (verbose) {
        mktitle("p2")
        print(p2)
        print(p2i)
    }

    # Dates, two ways:
    p2_query_dates <- as.Date(c(
        "1905-01-01", "1915-01-03", "1915-06-01", "1970-01-01"
    ))
    q2a <- datetimefunc$query_pulsetable_dates(
        pulsetable = p2,
        query_dates = p2_query_dates,
        query_pulsetable_fn = datetimefunc$query_pulsetable_times_v1
    )
    q2b <- datetimefunc$query_pulsetable_dates(
        pulsetable = p2,
        query_dates = p2_query_dates,
        query_pulsetable_fn = datetimefunc$query_pulsetable_times_v2
    )
    if (verbose) {
        mktitle("q2a")
        print(q2a)
        mktitle("q2b")
        print(q2b)
    }
    datetimefunc$ensure_two_tables_equal(q2a, q2b)

    # -------------------------------------------------------------------------
    # 3
    # -------------------------------------------------------------------------

    # An empty one:
    p3 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(),
        event_durations = 10
    )
    p3i <- datetimefunc$mk_intervaltable_from_pulsetable(p3)
    if (verbose) {
        mktitle("p3")
        print(p3)
        print(p3i)
    }
    # Empty ones:
    q3a <- datetimefunc$query_pulsetable_times_v1(p3, p1_test_times)
    q3b <- datetimefunc$query_pulsetable_times_v2(p3, p1_test_times)
    if (verbose) {
        mktitle("q3a")
        print(q3a)
        mktitle("q3b")
        print(q3b)
    }
    datetimefunc$ensure_two_tables_equal(q3a, q3b)

    # -------------------------------------------------------------------------
    # 4
    # -------------------------------------------------------------------------

    # Another empty one:
    p4 <- datetimefunc$mk_pulsetable_dates(
        origin_date = p2_origin_date,
        event_dates = c(),
        event_durations = lubridate::duration(300, units = "days"),
        time_units = p2_time_units
    )
    p4i <- datetimefunc$mk_intervaltable_from_pulsetable(p4)
    if (verbose) {
        mktitle("p4")
        print(p4)
        print(p4i)
    }
    # Empty ones:
    q4a <- datetimefunc$query_pulsetable_dates(
        p4,
        p2_query_dates
    )
    q4b <- datetimefunc$query_pulsetable_dates(
        p4,
        p2_query_dates,
        query_pulsetable_fn = datetimefunc$query_pulsetable_times_v2
    )
    if (verbose) {
        mktitle("q4a")
        print(q4a)
        mktitle("q4b")
        print(q4b)
    }
    datetimefunc$ensure_two_tables_equal(q4a, q4b)

    # -------------------------------------------------------------------------
    # 5
    # -------------------------------------------------------------------------

    # A single-event one:
    p5 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(7),
        event_durations = 10
    )
    p5i <- datetimefunc$mk_intervaltable_from_pulsetable(p5)
    if (verbose) {
        mktitle("p5")
        print(p5)
        print(p5i)
    }
    # Single-event ones:
    q5a <- datetimefunc$query_pulsetable_times_v1(p5, p1_test_times)
    q5b <- datetimefunc$query_pulsetable_times_v2(p5, p1_test_times)
    if (verbose) {
        mktitle("q5a")
        print(q5a)
        mktitle("q5b")
        print(q5b)
    }
    datetimefunc$ensure_two_tables_equal(q5a, q5b)

    # -------------------------------------------------------------------------
    # 6
    # -------------------------------------------------------------------------

    # Multiple event durations:
    p6 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times =     c(5,  20, 100, 105, 150, 200),
        event_durations = c(10, 20,  30,  30,  20,  20)
        # checked: it fails with e.g. length 2
    )
    p6i <- datetimefunc$mk_intervaltable_from_pulsetable(p6)
    if (verbose) {
        mktitle("p6")
        print(p6)
        print(p6i)
    }
    q6a <- datetimefunc$query_pulsetable_times_v1(p6, p1_test_times)
    q6b <- datetimefunc$query_pulsetable_times_v2(p6, p1_test_times)
    if (verbose) {
        mktitle("q6a")
        print(q6a)
        mktitle("q6b")
        print(q6b)
    }
    datetimefunc$ensure_two_tables_equal(q6a, q6b)

    # -------------------------------------------------------------------------
    # 7
    # -------------------------------------------------------------------------

    # Multiple event durations, different order; plus a "duplicate" at 105:
    p7data <- tibble(
        event_times =     c(5,  100, 20, 105, 105, 150, 200),
        event_durations = c(10,  30, 20,  30,   5,  20,  20)
    )
    p7data_sorted <- p7data %>% arrange(event_times, event_durations)
    p7 <- datetimefunc$mk_pulsetable_dimensionless(
        # as for p6 but with two flipped
        event_times = p7data$event_times,
        event_durations = p7data$event_durations
    )
    p7i <- datetimefunc$mk_intervaltable_from_pulsetable(p7)
    if (verbose) {
        mktitle("p7")
        print(p7)
        print(p7i)
    }
    q7a <- datetimefunc$query_pulsetable_times_v1(p7, p1_test_times)
    q7b <- datetimefunc$query_pulsetable_times_v2(p7, p1_test_times)
    stopifnot(isTRUE(all.equal(p7$event_times, p7data_sorted$event_times)))
    stopifnot(isTRUE(all.equal(p7$event_durations, p7data_sorted$event_durations)))
    datetimefunc$ensure_two_tables_equal(q7a, q7b)
}


datetimefunc$test_pulsecalcs <- function() {
    # Show plots to test our pulse event calculation functions.

    mkdataplot <- function(t, event_times, point_event_duration, with_dates) {
        if (with_dates) {
            origin_date <- min(t)
            time_units <- "days"
            pulsetable <- datetimefunc$mk_pulsetable_dates(
                origin_date = origin_date,
                event_dates = event_times,
                event_durations = point_event_duration,
                time_units = time_units
            )
            q <- datetimefunc$query_pulsetable_dates(
                pulsetable = pulsetable,
                query_dates = t
            )
        } else {
            pulsetable <- datetimefunc$mk_pulsetable_dimensionless(
                event_times = event_times,
                event_durations = point_event_duration
            )
            q <- datetimefunc$query_pulsetable_times(pulsetable, t)
        }
        d <- (
            q
            %>% mutate(
                event_start = ifelse(t %in% event_times, 1, 0),
                ever = as.numeric(ever),
                current = as.numeric(current),
                hx = as.numeric(hx)
            )
            %>% pivot_longer(
                cols = !any_of(c("t", "t_raw")),
                # ... any_of() doesn't complain about nonexistent columns,
                # unlike all_of().
                names_to = "quantity",
                values_to = "y"
            )
        )
        p <- (
            ggplot(d, aes(x = t, y = y))
            + geom_line()
            + ylim(0, NA)
            + ylab("")
            + facet_grid(quantity ~ ., scales = "free")
        )
        return(list(d = d, p = p))
    }

    with_abstract_times <- mkdataplot(
        t = seq(0, 70, by = 0.1),
        event_times = c(10, 20, 30, 32, 40, 50),
        point_event_duration = 3,
        with_dates = FALSE
    )
    with_dates <- mkdataplot(
        t = seq(
            lubridate::ymd("2000-01-01"),
            lubridate::ymd("2000-12-31"),
            by = "1 day"
        ),
        event_times = c(
            lubridate::ymd("2000-03-01"),
            lubridate::ymd("2000-04-01"),
            lubridate::ymd("2000-05-01"),
            lubridate::ymd("2000-05-05"),
            lubridate::ymd("2000-06-01"),
            lubridate::ymd("2000-07-01")
        ),
        point_event_duration = lubridate::duration(1, "week"),
        with_dates = TRUE
    )
    combined_plot <- (with_abstract_times$p | with_dates$p)
    # print(combined_plot)

    return(list(
        demo_time_fig = with_abstract_times$p,
        demo_time_date_fig = combined_plot
    ))
}


# =============================================================================
# Event concatenation
# =============================================================================

# -----------------------------------------------------------------------------
# Merge events: dimensionless
# -----------------------------------------------------------------------------

datetimefunc$merge_events_dimensionless_v1 <- function(
    event_times,
    event_ends,
    max_permitted_gap = 0,
    with_checks = FALSE
) {
    # For a series of events, defined by their start/end times, merge
    # contiguous or overlapping events.
    #
    # Arguments:
    #   event_times
    #       Vector of dimensionless start times.
    #   event_ends
    #       Corresponding vector of dimensionless end times.
    #   max_permitted_gap
    #       Permit up to this much (dimensionless) time between two episodes
    #       and still consider it one episode. (Typical use: a referral ends on
    #       Tuesday, the next starts on Wednesday to a different team, and we
    #       want to consider that one episode of care.)
    #   with_checks
    #       Validate inputs? Faster not to.
    #
    # Returns:
    #       A tibble with columns "start" and "end" representing contiguous,
    #       amalgamated, sorted episodes.

    n_events <- length(event_times)
    if (with_checks) {
        stopifnot(all(is.finite(event_times)))  # excludes NAs (but 0-length OK)
        stopifnot(length(event_ends) == n_events)
        stopifnot(all(is.finite(event_ends)))
        stopifnot(all(event_times <= event_ends))
        stopifnot(max_permitted_gap >= 0)
    }
    if (n_events == 0) {
        return(tibble(
            start = numeric(),
            end = numeric()
        ))
    }

    d <- (
        tibble(start = event_times, end = event_ends)
        %>% arrange(start)
    )
    n <- nrow(d)  # number of events
    accumulated_starts <- NULL
    accumulated_ends <- NULL
    current_start <- NA
    current_end <- NA
    for (i in 1:n) {  # iterate through events
        s <- d$start[i]
        e <- d$end[i]
        if (is.na(current_start)) {
            # Starting a new episode.
            current_start <- s
            current_end <- e
        }
        if (is.na(current_end)) {
            # We have found a referral that does not end. We're done.
            break
        }
        # Otherwise: does the new referral extend our end date?
        if (s <= current_end + max_permitted_gap) {
            # Treat as contiguous
            current_end <- max(current_end, e)
            # If e is NA, this will give NA.
            if (is.na(current_end)) {
                # We have found a referral that does not end. We're done.
                break
            }
            # If we get here, the next loop iteration will inspect the next
            # referral in this sequence, and maybe join it to ours.
        } else {
            # New referral (s, e) is too far in the future; it's not contiguous
            # with (current_start, current_end). This episode is done. But
            # there may be more.
            accumulated_starts <- c(accumulated_starts, current_start)
            accumulated_ends <- c(accumulated_ends, current_end)
            current_start <- s
            current_end <- e
        }
    }
    # "break" commands come here
    accumulated_starts <- c(accumulated_starts, current_start)
    accumulated_ends <- c(accumulated_ends, current_end)
    return(tibble(
        start = accumulated_starts,
        end = accumulated_ends
    ))
}


datetimefunc$merge_events_dimensionless_v2 <- function(
    event_times,
    event_ends,
    max_permitted_gap = 0,
    with_checks = FALSE
) {
    # As for merge_events_dimensionless(), but via a different method. It turns
    # out this is the SLOWER of two methods; see merge_events_dimensionless().
    # Arguments are as before.

    n_events <- length(event_times)
    if (with_checks) {
        stopifnot(all(is.finite(event_times)))  # excludes NAs (but 0-length OK)
        stopifnot(length(event_ends) == n_events)
        stopifnot(all(is.finite(event_ends)))
        stopifnot(all(event_times <= event_ends))
        stopifnot(max_permitted_gap >= 0)
    }
    if (n_events == 0) {
        return(tibble(
            start = numeric(),
            end = numeric()
        ))
    }

    episodes <- (
        tibble(start = event_times, end = event_ends)
        %>% arrange(start, end)
    )
    if (n_events >= 2) {
        prev_event_ends <- c(-Inf, episodes$end[1 : (n_events - 1)])
    } else {
        prev_event_ends <- -Inf
    }
    episodes <- (
        episodes
        %>% mutate(
            prev_event_ends = prev_event_ends,
            prev_event_ends_with_gap = prev_event_ends + max_permitted_gap,
            new_group = if_else(start > prev_event_ends_with_gap, 1, 0),
            groupnum = cumsum(new_group)
        )
        %>% group_by(groupnum)
        %>% summarize(
            start = min(start),
            end = max(end)
        )
        %>% ungroup()
        %>% select(start, end)
    )
    return(episodes)
}


datetimefunc$merge_events_dimensionless_v3 <- function(
    event_times,
    event_ends,
    max_permitted_gap = 0,
    with_checks = FALSE
) {
    # Currently the fastest version of this function.

    n_events <- length(event_times)
    if (with_checks) {
        stopifnot(all(is.finite(event_times)))  # excludes NAs (but 0-length OK)
        stopifnot(length(event_ends) == n_events)
        stopifnot(all(is.finite(event_ends)))
        stopifnot(all(event_times <= event_ends))
        stopifnot(max_permitted_gap >= 0)
    }
    if (n_events == 0) {
        return(tibble(start = numeric(), end = numeric()))
    }

    ordering <- order(event_times, event_ends)
    start <- event_times[ordering]
    end <- event_ends[ordering]

    if (n_events >= 2) {
        prev_event_ends <- c(-Inf, end[1 : (n_events - 1)])
    } else {
        prev_event_ends <- -Inf
    }

    prev_event_ends_with_gap <- prev_event_ends + max_permitted_gap

    new_group <- ifelse(start > prev_event_ends_with_gap, 1, 0)
    episodes <- data.table(
        start = start,
        end = end,
        groupnum = cumsum(new_group)
    )[, .(start = min(start), end = max(end)), by = .(groupnum)]
    episodes[, groupnum := NULL]
    return(as_tibble(episodes))
}


# The fastest:
datetimefunc$merge_events_dimensionless <- datetimefunc$merge_events_dimensionless_v3


# -----------------------------------------------------------------------------
# Merge events: dates
# -----------------------------------------------------------------------------

datetimefunc$merge_events_dates <- function(
    event_times,
    event_ends,
    max_permitted_gap = lubridate::days(0),
    merge_event_fn = datetimefunc$merge_events_dimensionless
) {
    # As for merge_events_dimensionless(), but using dates.

    # This does need a separate function (or at least some acknowledgement that
    # the units are dates/periods); otherwise, concatenation with -Inf turns
    # everything numeric and then breaks some code. So we'll turn everything
    # numeric explicitly.
    #
    # Also, by default, conversion to numeric uses seconds for date-time
    # objects and days for dates; we'll force everything to dates first to
    # avoid error.
    event_times <- as.numeric(as.Date(event_times))  # days
    event_ends <- as.numeric(as.Date(event_ends))  # days
    max_permitted_gap <- lubridate::time_length(max_permitted_gap, unit = "days")
    episodes_dimensionless <- merge_event_fn(
        event_times = event_times,
        event_ends = event_ends,
        max_permitted_gap = max_permitted_gap
    )
    return (
        episodes_dimensionless
        %>% mutate(
            start = as.Date(start),
            end = as.Date(end)
        )
    )
}


# -----------------------------------------------------------------------------
# Merge events: testing
# -----------------------------------------------------------------------------

datetimefunc$test_merge_events <- function(verbose = TRUE) {
    line <- paste(rep("=", 79), collapse = "")
    mktitle <- function(x) {
        cat(line, "\n- ", x, "\n", line, "\n", sep = "")
    }

    # -------------------------------------------------------------------------
    # 1: dimensionless, gap
    # -------------------------------------------------------------------------

    e1_starts <- c(5, 20, 100, 105, 150, 200, 250, 256, 300, 305, 400)
    e1_ends <-   c(7, 30, 105, 149, 170, 210, 260, 290, 310, 350, 410)
    e1_maxgap <- 1

    e1a <- datetimefunc$merge_events_dimensionless_v1(
        event_times = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e1_maxgap
    )
    e1b <- datetimefunc$merge_events_dimensionless_v2(
        event_times = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e1_maxgap
    )
    e1c <- datetimefunc$merge_events_dimensionless_v2(
        event_times = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e1_maxgap
    )
    if (verbose) {
        mktitle("e1")
        cat("e1_starts:\n")
        print(e1_starts)
        cat("e1_ends:\n")
        print(e1_ends)
        cat("e1_maxgap:\n")
        print(e1_maxgap)
        cat("e1a:\n")
        print(e1a)
        cat("e1b:\n")
        print(e1b)
        cat("e1b:\n")
        print(e1c)
    }
    datetimefunc$ensure_two_tables_equal(e1a, e1b)
    datetimefunc$ensure_two_tables_equal(e1a, e1c)

    # Speed test
    mktitle("Speed test")
    bmk <- microbenchmark::microbenchmark(
        merge_events_dimensionless_v1 = {
            datetimefunc$merge_events_dimensionless_v1(
                event_times = e1_starts,
                event_ends = e1_ends,
                max_permitted_gap = e1_maxgap
            )
        },
        merge_events_dimensionless_v2 = {
            datetimefunc$merge_events_dimensionless_v2(
                event_times = e1_starts,
                event_ends = e1_ends,
                max_permitted_gap = e1_maxgap
            )
        },
        merge_events_dimensionless_v3 = {
            datetimefunc$merge_events_dimensionless_v3(
                event_times = e1_starts,
                event_ends = e1_ends,
                max_permitted_gap = e1_maxgap
            )
        },
        times = 1000
    )
    print(bmk)
    plot(bmk)

    # -------------------------------------------------------------------------
    # 2: dimensionless, no gap
    # -------------------------------------------------------------------------

    e2_maxgap <- 0
    e2a <- datetimefunc$merge_events_dimensionless_v1(
        event_times = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e2_maxgap
    )
    e2b <- datetimefunc$merge_events_dimensionless_v2(
        event_times = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e2_maxgap
    )
    e2c <- datetimefunc$merge_events_dimensionless_v3(
        event_times = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e2_maxgap
    )
    if (verbose) {
        mktitle("e2")
        cat("e2_maxgap:\n")
        print(e2_maxgap)
        cat("e2a:\n")
        print(e2a)
        cat("e2b:\n")
        print(e2b)
        cat("e2c:\n")
        print(e2c)
    }
    datetimefunc$ensure_two_tables_equal(e2a, e2b)
    datetimefunc$ensure_two_tables_equal(e2a, e2c)

    # -------------------------------------------------------------------------
    # 3: dates, small gap
    # -------------------------------------------------------------------------

    e3_starts <- as.Date(c(
        "2020-01-05", "2020-01-20", "2020-06-01",
        "2020-06-05", "2020-08-01", "2020-12-01"
    ))
    e3_ends <-   as.Date(c(
        "2020-01-12", "2020-01-30", "2020-07-01",
        "2020-07-31", "2020-08-30", "2020-12-25"
    ))
    e3_maxgap <- lubridate::days(1)

    e3a <- datetimefunc$merge_events_dates(
        event_times = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e3_maxgap
    )
    e3b <- datetimefunc$merge_events_dates(
        event_times = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e3_maxgap,
        merge_event_fn = datetimefunc$merge_events_dimensionless_v2
    )
    if (verbose) {
        mktitle("e3")
        cat("e3_starts:\n")
        print(e3_starts)
        cat("e3_ends:\n")
        print(e3_ends)
        cat("e3_maxgap:\n")
        print(e3_maxgap)
        cat("e3a:\n")
        print(e3a)
        cat("e3b:\n")
        print(e3b)
    }
    datetimefunc$ensure_two_tables_equal(e3a, e3b)

    # -------------------------------------------------------------------------
    # 4: dates, no gap
    # -------------------------------------------------------------------------

    e4_maxgap <- lubridate::days(0)

    e4a <- datetimefunc$merge_events_dates(
        event_times = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e4_maxgap
    )
    e4b <- datetimefunc$merge_events_dates(
        event_times = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e4_maxgap,
        merge_event_fn = datetimefunc$merge_events_dimensionless_v2
    )
    if (verbose) {
        mktitle("e4")
        cat("e4_maxgap:\n")
        print(e4_maxgap)
        cat("e4a:\n")
        print(e4a)
        cat("e4b:\n")
        print(e4b)
    }
    datetimefunc$ensure_two_tables_equal(e4a, e4b)

    # -------------------------------------------------------------------------
    # 5: dimensionless, empty
    # -------------------------------------------------------------------------

    e5a <- datetimefunc$merge_events_dimensionless_v1(
        event_times = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    e5b <- datetimefunc$merge_events_dimensionless_v2(
        event_times = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    e5c <- datetimefunc$merge_events_dimensionless_v3(
        event_times = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    if (verbose) {
        mktitle("e5")
        cat("e5a:\n")
        print(e5a)
        cat("e5b:\n")
        print(e5b)
        cat("e5c:\n")
        print(e5c)
    }
    datetimefunc$ensure_two_tables_equal(e5a, e5b)
    datetimefunc$ensure_two_tables_equal(e5a, e5c)

    # -------------------------------------------------------------------------
    # 6: dates, empty
    # -------------------------------------------------------------------------

    e6a <- datetimefunc$merge_events_dates(
        event_times = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    e6b <- datetimefunc$merge_events_dates(
        event_times = c(),
        event_ends = c(),
        max_permitted_gap = 0,
        merge_event_fn = datetimefunc$merge_events_dimensionless_v2
    )
    if (verbose) {
        mktitle("e6")
        cat("e6a:\n")
        print(e6a)
        cat("e6b:\n")
        print(e6b)
    }
    datetimefunc$ensure_two_tables_equal(e6a, e6b)

    # -------------------------------------------------------------------------
    # 7: dimensionless, single row
    # -------------------------------------------------------------------------

    e7_starts <- 100
    e7_ends <- 120
    e7_maxgap <- 0
    e7a <- datetimefunc$merge_events_dimensionless_v1(
        event_times = e7_starts,
        event_ends = e7_ends,
        max_permitted_gap = e7_maxgap
    )
    e7b <- datetimefunc$merge_events_dimensionless_v2(
        event_times = e7_starts,
        event_ends = e7_ends,
        max_permitted_gap = e7_maxgap
    )
    e7c <- datetimefunc$merge_events_dimensionless_v3(
        event_times = e7_starts,
        event_ends = e7_ends,
        max_permitted_gap = e7_maxgap
    )
    if (verbose) {
        mktitle("e7")
        cat("e7a:\n")
        print(e7a)
        cat("e7b:\n")
        print(e7b)
        cat("e7b:\n")
        print(e7c)
    }
    datetimefunc$ensure_two_tables_equal(e7a, e7b)
    datetimefunc$ensure_two_tables_equal(e7a, e7c)
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("datetimefunc" %in% search()) detach("datetimefunc")
attach(datetimefunc)  # subsequent additions not found, so attach at the end
