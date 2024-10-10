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
        dplyr,
        ggplot2,
        lubridate,
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


datetimefunc$mk_pulsetable_dimensionless <- function(
    event_times,
    duration_per_event,
    include_interval_table = FALSE,
    merge_interval_table = FALSE
) {
    # Creates a "pulsetable" object, documenting events for a single subject,
    # with a presumed duration for each. This object can then be queried, e.g.
    # for dates, via query_pulsetable_ever(), query_pulsetable_times(), and
    # query_pulsetable_dates().
    #
    # This version of the function creates a dimensionless pulsetable object.
    # See also mk_pulsetable_dates().
    #
    # Arguments:
    #
    #   event_times
    #       Numeric times (e.g. ages in years) at which an event (e.g. lithium
    #       administration) occurred.
    #   duration_per_event
    #       Numeric time that each event is presumed to have endured for.
    #       Either a single number, or a vector as long as event_times.
    #       Consecutive events can "merge". This is not additive; for example,
    #       if event duration is 10 and events occur at times 20 and 25, then
    #       this will be treated as an event starting at time 20 and enduring
    #       until time 35.
    #   merge_interval_table
    #       Merge consecutive contiguous events? Only applicable for the
    #       interval table. (Pulse calculations are unaffected by such a
    #       merge.)
    #
    # Returns a list, with elements:
    #
    #   event_starts
    #       As for the input (but sorted and unique).
    #   duration_per_event
    #       As for the input.
    #   intervals
    #       If include_interval_table == FALSE, NULL. Otherwise: a tibble,
    #       containing one time interval per, collectively covering all time
    #       from -Inf to +Inf with no gaps. Columns are:
    #
    #       t_start
    #           Numeric time at which each interval starts.
    #       t_end
    #           Numeric time at which each interval ends. The interval is
    #           represented by [start, end), i.e. inclusive start, exclusive
    #           end.
    #       event
    #           Is the event occurring (TRUE) or not (FALSE) during this
    #           interval?
    #       duration
    #           Interval duration.
    #       event_duration
    #           Interval duration if the event is occurring, or 0 if it's not.
    #       cum_event_dur
    #           Cumulative duration, to the END of the interval, spent "during"
    #           events.
    #   origin_date
    #       The date from which times are calculated, or NA if this object
    #       is not using dates.
    #   time_units
    #       Units of time (e.g. "years" or NA).

    # Argument checks
    n_event_times <- length(event_times)
    n_durations <- length(duration_per_event)
    stopifnot(
        all(is.finite(event_times))  # excludes NA values (but 0-length OK)
        && (n_durations == 1 || n_durations == n_event_times)
        && all(is.finite(duration_per_event))
        && all(duration_per_event > 0)
    )

    event_starts <- sort(unique(event_times))
    n_events <- length(event_starts)
    if (n_events != n_event_times && n_durations > 1) {
        cat("--- BAD event_times:\n")
        dput(event_times)
        stop(paste0(
            "You mustn't supply non-unique event_times if you are providing ",
            "individual values of duration_per_event."
        ))
    }

    # Create intervals
    # (a) "Event occurring" intervals
    if (include_interval_table) {
        n_events <- length(event_starts)
        if (n_events == 0) {
            intervals_occurring <- NULL
            intervals_not_occurring <- tibble(
                t_start = -Inf,
                t_end = Inf,
                event = FALSE
            )
        } else {
            if (n_events >= 2) {
                next_event_starts <- c(event_starts[2 : n_events], Inf)
            } else {
                next_event_starts <- Inf
            }
            event_ends <- pmin(event_starts + duration_per_event, next_event_starts)
            # ... up until the expiry of this event, or until the next event starts

            # (b) Merge consecutive contiguous events, if required.
            if (merge_interval_table) {
                if (n_events >= 2) {
                    prev_event_ends <- c(-Inf, event_ends[1 : (n_events - 1)])
                } else {
                    prev_event_ends <- -Inf
                }
                intervals_occurring <- (
                    tibble(
                        t_start = event_starts,
                        t_end = event_ends,
                        prev_event_ends = prev_event_ends
                    )
                )
                intervals_occurring <- (
                    intervals_occurring
                    %>% mutate(
                        new_group = if_else(t_start > prev_event_ends, 1, 0),
                        groupnum = cumsum(new_group)
                    )
                    %>% group_by(groupnum)
                    %>% summarize(
                        t_start = min(t_start),
                        t_end = max(t_end)
                    )
                    %>% ungroup()
                    %>% select(-groupnum)
                    %>% mutate(event = TRUE)
                )
            } else {
                intervals_occurring <- tibble(
                    t_start = event_starts,
                    t_end = event_ends,
                    event = TRUE
                )
            }

            # (c) Now add in "non-occurring" events: the gaps.
            # The intervals are non-overlapping.
            intervals_not_occurring <- (
                tibble(
                    t_start = c(-Inf, intervals_occurring$t_end),
                    t_end = c(intervals_occurring$t_start, Inf),
                    event = FALSE
                )
                %>% filter(t_start < t_end)  # eliminates dummy non-event intervals
            )
        }

        # (d) Combine and finish off calculations.
        intervals <- (
            rbind(intervals_occurring, intervals_not_occurring)
            %>% arrange(t_start)
            %>% mutate(
                duration = t_end - t_start,
                event_duration = ifelse(event, duration, 0)
                # ... don't multiply; duration may be infinite
            )
            %>% mutate(
                cum_event_dur = cumsum(event_duration)
            )
        )
    } else {
        intervals <- NULL
    }
    return(list(
        event_starts = event_starts,
        duration_per_event = duration_per_event,
        intervals = intervals,
        origin_date = lubridate::NA_Date_,
        time_units = NA_character_
    ))
}


datetimefunc$mk_pulsetable_dates <- function(
    origin_date,
    event_dates,
    duration_per_event,
    time_units = "years",
    include_interval_table = FALSE
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
    #   duration_per_event
    #       Should be a lubridate duration in a sensible (constant) unit, e.g.
    #       lubridate::duration(30, units = "days") -- or a vector of these, of
    #       the same length as event_dates. Though lubridate does use
    #       constants, e.g. 365.25 days per year for "years" (see
    #       ?lubridate::duration), so "years" is fine.
    #   time_units
    #       Textual units (e.g. "days", "years") to operate with internally.
    #       As used by the lubridate package.
    #   include_interval_table
    #       As for mk_pulsetable_dimensionless().
    #
    # Returns:
    #
    #   As for mk_pulsetable_dimensionless(). However, if
    #   include_interval_table == TRUE, the following columns are also added to
    #   the "intervals" element:
    #
    #       t_start_date
    #           Date version of t_start.
    #       t_end_date
    #           Date version of t_end.

    if (!(length(origin_date) == 1
            && lubridate::is.Date(origin_date))) {
        stop("Bad origin_date parameter: ", origin_date)
    }
    if (!(length(event_dates) == 0
            || all(lubridate::is.Date(event_dates)))) {
        stop("Bad event_dates parameter: ", event_dates)
    }
    if (!(
        (length(duration_per_event) == 1
            || length(duration_per_event) == length(event_dates))
        && all(lubridate::is.duration(duration_per_event))
    )) {
        stop("Bad duration_per_event parameter: ", duration_per_event)
    }
    pt <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = datetimefunc$duration_units(
            start_date = origin_date,
            end_date = event_dates,
            units = time_units
        ),
        duration_per_event = (
            duration_per_event /
            lubridate::duration(1, units = time_units)
        ),
        include_interval_table = include_interval_table
    )
    if (include_interval_table) {
        pt$intervals <- (
            pt$intervals
            %>% mutate(
                t_start_date = as.Date(
                    origin_date
                    + lubridate::duration(t_start, units = time_units)
                ),
                t_end_date = as.Date(
                    origin_date
                    + lubridate::duration(t_end, units = time_units)
                )
                # A date plus "1 day" gives a date, but a date plus "1 year"
                # gives a datetime, so we'll force to a date.
            )
        )
    }
    pt$origin_date <- origin_date
    pt$time_units <- time_units
    return(pt)
}


datetimefunc$query_pulsetable_ever <- function(pt) {
    # Queries a pulsetable in a very basic way: "Did the event ever occur?"
    return(any(pt$intervals$event))
}


datetimefunc$query_pulsetable_times <- function(pulsetable, query_times) {
    # Query a pulse table (see above -- relating to a single subject) at
    # various times (in the dimensionless time units used within the
    # pulsetable).
    #
    # This version operates in parallel and is sufficiently fast.
    #
    # Arguments:
    #
    #   pulsetable
    #       The pulse table object (list, as above) to query.
    #   query_times
    #       The (dimensionless) times at which to produce a row in the output.
    #
    # Returns a tibble with columns:
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
    #   ever
    #       Does the event ever occur for the subject (across the lifetime,
    #       including in the future)? Boolean. Obviously, this value will be
    #       the same for all values of t.

    stopifnot(all(is.numeric(query_times)))
    # ... prevents NA values; also fails for empty input
    event_starts <- pulsetable$event_starts  # sorted, but may be empty
    duration_per_event <- pulsetable$duration_per_event
    # ... a single number or a vector matching event_starts
    n_events <- length(event_starts)
    n_durations <- length(duration_per_event)

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

        indexes_danger <- findInterval(query_times, event_starts)
        # ... Defaults are correct; see ?findInterval and its first example:
        #   rightmost.closed = FALSE
        #   all.inside = FALSE
        #   left.open = FALSE
        # If the "x" values are GREATER THAN OR EQUAL TO the test values
        # ("vec"), they get assigned the index of the (greatest such) test
        # value.

        # In what follows, beware the following. The expression
        # event_starts[indexes_danger] cannot be used, because indexes_danger
        # contains 0 values (an invalid index) -- and the output vector in that
        # circumstance is of a DIFFERENT LENGTH and therefore garbled. Similar
        # if you use NA instead, etc. Core syntax is:
        # https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Index-vectors
        # Therefore, we must create indexes_safer, but only use its lookup
        # values when indexes_danger != 0.
        invalid_indexes <- indexes_danger == 0
        indexes_safer <- ifelse(invalid_indexes, 1, indexes_danger)
        relevant_starts <- event_starts[indexes_safer]
        if (n_durations > 1) {
            relevant_durations <- duration_per_event[indexes_safer]
        } else {
            relevant_durations <- duration_per_event  # single number
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
        first_event <- min(event_starts)
        hx <- query_times >= first_event

        # ---------------------------------------------------------------------
        # t_since_first
        # ---------------------------------------------------------------------
        t_since_first <- pmax(0, query_times - first_event)

        # ---------------------------------------------------------------------
        # cum_t_on
        # ---------------------------------------------------------------------
        if (n_events >= 2) {
            event_ends <- pmin(  # until the first of:
                event_starts + duration_per_event,  # this event ends...
                c(event_starts[2 : n_events], Inf)  # ... or the next one starts
            )
            event_durations <- event_ends - event_starts
            cum_durations <- cumsum(event_durations)
            cum_duration_at_start <- c(0, cum_durations[1 : n_events - 1])
        } else {
            # n_events is 1
            cum_duration_at_start <- 0
        }
        cum_t_on <- ifelse(
            invalid_indexes,  # query time before first start time?
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

        # ---------------------------------------------------------------------
        # cum_t_after
        # ---------------------------------------------------------------------
        cum_t_after <- t_since_first - cum_t_on

        # cat("- query_pulsetable_times():\n")
        # cat("... query_times:\n"); print(query_times)
        # cat("... event_starts:\n"); print(event_starts)
        # cat("... duration_per_event:\n"); print(duration_per_event)
        # cat("... indexes_danger:\n"); print(indexes_danger)
        # cat("... invalid_indexes:\n"); print(invalid_indexes)
        # cat("... indexes_safer:\n"); print(indexes_safer)
        # cat("... current:\n"); print(current)
        # cat("... event_durations:\n"); print(event_durations)
        # cat("... cum_durations:\n"); print(cum_durations)
        # cat("... cum_duration_at_start:\n"); print(cum_duration_at_start)
        # cat("... cum_t_on:\n"); print(cum_t_on)

    } else {
        # never!
        current <- FALSE
        hx <- FALSE
        t_since_first <- 0
        cum_t_on <- 0
        cum_t_after <- 0
    }

    return(tibble(
        t = query_times,
        current = current,
        hx = hx,
        t_since_first = t_since_first,
        cum_t_on = cum_t_on,
        cum_t_after = cum_t_after,
        ever = ever
    ))
}


datetimefunc$query_pulsetable_times_slow <- function(pulsetable, query_times) {
    # See query_pulsetable_times() above. This version is TOO SLOW: e.g. 19.3
    # seconds for 1000 interations with p1/p1_test_times as above.
    stopifnot(all(is.numeric(query_times)))  # also fails for empty input
    intervals <- pulsetable$intervals
    if (is.null(intervals)) {
        stop(paste0(
            "Must use create_interval_table = TRUE argument to ",
            "mk_pulsetable_dimensionless() or mk_pulsetable_dates() to use ",
            "this version of the function."
        ))
    }
    ever <- any(intervals$event)
    query_fn <- function(t) {  # t is a SINGLE VALUE.
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
        }
        return(tibble(
            t = t,
            current = current,
            hx = hx,
            t_since_first = t_since_first,
            cum_t_on = cum_t_on,
            cum_t_after = cum_t_after
        ))
    }
    result <- (
        tibble(t = query_times)
        %>% rowwise()
        %>% reframe(query_fn(t))
        %>% mutate(ever = ever)
    )
    return(result)
}


datetimefunc$query_pulsetable_dates <- function(
    pulsetable,
    query_dates,
    use_slow = FALSE
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
    #   use_slow
    #       Use a slow method (for cross-checking). Should NOT be true for
    #       production code.
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
    if (use_slow) {
        q <- datetimefunc$query_pulsetable_times_slow(
            pulsetable = pulsetable,
            query_times = query_times
        )
    }
    else {
        q <- datetimefunc$query_pulsetable_times(
            pulsetable = pulsetable,
            query_times = query_times
        )
    }
    return(q %>% mutate(t_raw = t, t = query_dates))
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

    p1 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(5, 20, 100, 105, 150, 200),
        duration_per_event = 10,
        include_interval_table = TRUE
    )
    if (verbose) {
        mktitle("p1")
        print(p1)
    }

    # Very basic query:
    q1ever <- datetimefunc$query_pulsetable_ever(p1)
    if (verbose) {
        mktitle("q1ever")
        print(q1ever)
    }

    # Times, two ways:
    p1_test_times <- c(0, 5, 7, 17, 50, 103, 115, 500)
    q1a <- datetimefunc$query_pulsetable_times(p1, p1_test_times)
    q1b <- datetimefunc$query_pulsetable_times_slow(p1, p1_test_times)
    if (verbose) {
        mktitle("q1a")
        print(q1a)
        mktitle("q1b")
        print(q1b)
    }
    stopifnot(all.equal(q1a, q1b))
    # - use all.equal(), not identical(); the latter reports differences that
    #   are due to infinitesimal floating-point differences; the first doesn't
    #   care (appropriately).
    # - if discrepancy: print(q1a == q1b), then fix bug!

    # Speed test
    n_tests <- 1000
    mktitle("Speed test, query_pulsetable_times")
    tmp_start <- Sys.time()
    for (i in 1:n_tests) {
        datetimefunc$query_pulsetable_times(p1, p1_test_times)
    }
    tmp_end <- Sys.time()
    cat("- ", n_tests, " iterations took:\n", sep = "")
    print(tmp_end - tmp_start)
    # 19.3 seconds for query_pulsetable_times_slow
    # ~0.5 seconds for query_pulsetable_times
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
    # # ... about 1.14 s; so conditionality on variables is NOT helpful.

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
        duration_per_event = lubridate::duration(300, units = "days"),
        time_units = p2_time_units,
        include_interval_table = TRUE
    )
    if (verbose) {
        mktitle("p2")
        print(p2)
    }

    # Dates, two ways:
    p2_query_dates <- as.Date(c(
        "1905-01-01", "1915-01-03", "1915-06-01", "1970-01-01"
    ))
    q2a <- datetimefunc$query_pulsetable_dates(
        pulsetable = p2,
        query_dates = p2_query_dates
    )
    q2b <- datetimefunc$query_pulsetable_dates(
        pulsetable = p2,
        query_dates = p2_query_dates,
        use_slow = TRUE
    )
    if (verbose) {
        mktitle("q2a")
        print(q2a)
        mktitle("q2b")
        print(q2b)
    }
    stopifnot(all.equal(q2a, q2b))

    # -------------------------------------------------------------------------
    # 3
    # -------------------------------------------------------------------------

    # An empty one:
    p3 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(),
        duration_per_event = 10,
        include_interval_table = TRUE
    )
    if (verbose) {
        mktitle("p3")
        print(p3)
    }
    # Empty ones:
    q3a <- datetimefunc$query_pulsetable_times(p3, p1_test_times)
    q3b <- datetimefunc$query_pulsetable_times_slow(p3, p1_test_times)
    if (verbose) {
        mktitle("q3a")
        print(q3a)
        mktitle("q3b")
        print(q3b)
    }
    stopifnot(all.equal(q3a, q3b))

    # -------------------------------------------------------------------------
    # 4
    # -------------------------------------------------------------------------

    # Another empty one:
    p4 <- datetimefunc$mk_pulsetable_dates(
        origin_date = p2_origin_date,
        event_dates = c(),
        duration_per_event = lubridate::duration(300, units = "days"),
        time_units = p2_time_units,
        include_interval_table = TRUE
    )
    if (verbose) {
        mktitle("p4")
        print(p4)
    }
    # Empty ones:
    q4a <- datetimefunc$query_pulsetable_dates(p4, p2_query_dates)
    q4b <- datetimefunc$query_pulsetable_dates(p4, p2_query_dates,
                                               use_slow = TRUE)
    if (verbose) {
        mktitle("q4a")
        print(q4a)
        mktitle("q4b")
        print(q4b)
    }
    stopifnot(all.equal(q4a, q4b))

    # -------------------------------------------------------------------------
    # 5
    # -------------------------------------------------------------------------

    # A single-event one:
    p5 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times = c(7),
        duration_per_event = 10,
        include_interval_table = TRUE
    )
    if (verbose) {
        mktitle("p5")
        print(p5)
    }
    # Single-event ones:
    q5a <- datetimefunc$query_pulsetable_times(p5, p1_test_times)
    q5b <- datetimefunc$query_pulsetable_times_slow(p5, p1_test_times)
    if (verbose) {
        mktitle("q5a")
        print(q5a)
        mktitle("q5b")
        print(q5b)
    }
    stopifnot(all.equal(q5a, q5b))

    # -------------------------------------------------------------------------
    # 6
    # -------------------------------------------------------------------------

    # Multiple event durations:
    p6 <- datetimefunc$mk_pulsetable_dimensionless(
        event_times =        c(5,  20, 100, 105, 150, 200),
        duration_per_event = c(10, 20,  30,  30,  20,  20),
        # checked: it fails with e.g. length 2
        include_interval_table = TRUE
    )
    if (verbose) {
        mktitle("p6")
        print(p6)
    }
    q6a <- datetimefunc$query_pulsetable_times(p6, p1_test_times)
    q6b <- datetimefunc$query_pulsetable_times_slow(p6, p1_test_times)
    if (verbose) {
        mktitle("q6a")
        print(q6a)
        mktitle("q6b")
        print(q6b)
    }
    stopifnot(all.equal(q6a, q6b))
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
                duration_per_event = point_event_duration,
                time_units = time_units
            )
            q <- datetimefunc$query_pulsetable_dates(
                pulsetable = pulsetable,
                query_dates = t
            )
        } else {
            pulsetable <- datetimefunc$mk_pulsetable_dimensionless(
                event_times = event_times,
                duration_per_event = point_event_duration
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

datetimefunc$merge_events_dimensionless <- function(
    event_starts,
    event_ends,
    max_permitted_gap = 0
) {
    # For a series of events, defined by their start/end times, merge
    # contiguous or overlapping events.
    #
    # Arguments:
    #   event_starts
    #       Vector of dimensionless start times.
    #   event_ends
    #       Corresponding vector of dimensionless end times.
    #   max_permitted_gap
    #       Permit up to this much (dimensionless) time between two episodes
    #       and still consider it one episode. (Typical use: a referral ends on
    #       Tuesday, the next starts on Wednesday to a different team, and we
    #       want to consider that one episode of care.)
    #
    # Returns:
    #       A tibble with columns "start" and "end" representing contiguous,
    #       amalgamated, sorted episodes.
    #
    # As for merge_events_dimensionless_slow(), but using a different method.
    # It turns out this one is faster (perhaps surprisingly), by a factor of
    # about 4.

    n_events <- length(event_starts)
    stopifnot(all(is.finite(event_starts)))  # excludes NAs (but 0-length OK)
    stopifnot(length(event_ends) == n_events)
    stopifnot(all(is.finite(event_ends)))
    stopifnot(all(event_starts <= event_ends))
    stopifnot(max_permitted_gap >= 0)
    if (n_events == 0) {
        return(tibble(
            start = numeric(),
            end = numeric()
        ))
    }

    d <- (
        tibble(start = event_starts, end = event_ends)
        %>% arrange(start)
    )
    n <- nrow(d)  # number of events
    accumulated_starts <- NULL
    accumulated_ends <- NULL
    current_start <- NA
    current_end <- NA
    for (i in 1:n) {
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


datetimefunc$merge_events_dimensionless_slow <- function(
    event_starts,
    event_ends,
    max_permitted_gap = 0
) {
    # As for merge_events_dimensionless(), but via a different method. It turns
    # out this is the SLOWER of two methods; see merge_events_dimensionless().
    # Arguments are as before.

    n_events <- length(event_starts)
    stopifnot(all(is.finite(event_starts)))  # excludes NAs (but 0-length OK)
    stopifnot(length(event_ends) == n_events)
    stopifnot(all(is.finite(event_ends)))
    stopifnot(all(event_starts <= event_ends))
    stopifnot(max_permitted_gap >= 0)
    if (n_events == 0) {
        return(tibble(
            start = numeric(),
            end = numeric()
        ))
    }

    episodes <- (
        tibble(start = event_starts, end = event_ends)
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


datetimefunc$merge_events_dates <- function(
    event_starts,
    event_ends,
    max_permitted_gap = lubridate::days(0),
    use_slow = FALSE
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
    event_starts <- as.numeric(as.Date(event_starts))  # days
    event_ends <- as.numeric(as.Date(event_ends))  # days
    max_permitted_gap <- lubridate::time_length(max_permitted_gap, unit = "days")
    if (use_slow) {
        episodes_dimensionless <- datetimefunc$merge_events_dimensionless_slow(
            event_starts = event_starts,
            event_ends = event_ends,
            max_permitted_gap = max_permitted_gap
        )
    } else {
        episodes_dimensionless <- datetimefunc$merge_events_dimensionless(
            event_starts = event_starts,
            event_ends = event_ends,
            max_permitted_gap = max_permitted_gap
        )
    }
    return (
        episodes_dimensionless
        %>% mutate(
            start = as.Date(start),
            end = as.Date(end)
        )
    )
}


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

    e1a <- datetimefunc$merge_events_dimensionless(
        event_starts = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e1_maxgap
    )
    e1b <- datetimefunc$merge_events_dimensionless_slow(
        event_starts = e1_starts,
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
    }
    stopifnot(all.equal(e1a, e1b))

    # Speed test
    n_tests <- 1000
    mktitle("Speed test, merge_events_dimensionless/..._slow")
    tmp_start <- Sys.time()
    for (i in 1:n_tests) {
        dummy <- datetimefunc$merge_events_dimensionless(
            event_starts = e1_starts,
            event_ends = e1_ends,
            max_permitted_gap = e1_maxgap
        )
    }
    tmp_end <- Sys.time()
    cat("- FAST: ", n_tests, " iterations took:\n", sep = "")
    print(tmp_end - tmp_start)
    tmp_start <- Sys.time()
    for (i in 1:n_tests) {
        dummy <- datetimefunc$merge_events_dimensionless_slow(
            event_starts = e1_starts,
            event_ends = e1_ends,
            max_permitted_gap = e1_maxgap
        )
    }
    tmp_end <- Sys.time()
    cat("- SLOW: ", n_tests, " iterations took:\n", sep = "")
    print(tmp_end - tmp_start)

    # -------------------------------------------------------------------------
    # 2: dimensionless, no gap
    # -------------------------------------------------------------------------

    e2_maxgap <- 0
    e2a <- datetimefunc$merge_events_dimensionless(
        event_starts = e1_starts,
        event_ends = e1_ends,
        max_permitted_gap = e2_maxgap
    )
    e2b <- datetimefunc$merge_events_dimensionless_slow(
        event_starts = e1_starts,
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
    }
    stopifnot(all.equal(e2a, e2b))

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
        event_starts = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e3_maxgap
    )
    e3b <- datetimefunc$merge_events_dates(
        event_starts = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e3_maxgap,
        use_slow = TRUE
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
    stopifnot(all.equal(e3a, e3b))

    # -------------------------------------------------------------------------
    # 4: dates, no gap
    # -------------------------------------------------------------------------

    e4_maxgap <- lubridate::days(0)

    e4a <- datetimefunc$merge_events_dates(
        event_starts = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e4_maxgap
    )
    e4b <- datetimefunc$merge_events_dates(
        event_starts = e3_starts,
        event_ends = e3_ends,
        max_permitted_gap = e4_maxgap,
        use_slow = TRUE
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
    stopifnot(all.equal(e4a, e4b))

    # -------------------------------------------------------------------------
    # 5: dimensionless, empty
    # -------------------------------------------------------------------------

    e5a <- datetimefunc$merge_events_dimensionless(
        event_starts = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    e5b <- datetimefunc$merge_events_dimensionless_slow(
        event_starts = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    if (verbose) {
        mktitle("e5")
        cat("e5a:\n")
        print(e5a)
        cat("e5b:\n")
        print(e5b)
    }
    stopifnot(all.equal(e5a, e5b))

    # -------------------------------------------------------------------------
    # 6: dates, empty
    # -------------------------------------------------------------------------

    e6a <- datetimefunc$merge_events_dates(
        event_starts = c(),
        event_ends = c(),
        max_permitted_gap = 0
    )
    e6b <- datetimefunc$merge_events_dates(
        event_starts = c(),
        event_ends = c(),
        max_permitted_gap = 0,
        use_slow = TRUE
    )
    if (verbose) {
        mktitle("e6")
        cat("e6a:\n")
        print(e6a)
        cat("e6b:\n")
        print(e6b)
    }
    stopifnot(all.equal(e6a, e6b))

    # -------------------------------------------------------------------------
    # 7: dimensionless, single row
    # -------------------------------------------------------------------------

    e7_starts <- 100
    e7_ends <- 120
    e7_maxgap <- 0
    e7a <- datetimefunc$merge_events_dimensionless(
        event_starts = e7_starts,
        event_ends = e7_ends,
        max_permitted_gap = e7_maxgap
    )
    e7b <- datetimefunc$merge_events_dimensionless_slow(
        event_starts = e7_starts,
        event_ends = e7_ends,
        max_permitted_gap = e7_maxgap
    )
    if (verbose) {
        mktitle("e7")
        cat("e7a:\n")
        print(e7a)
        cat("e7b:\n")
        print(e7b)
    }
    stopifnot(all.equal(e7a, e7b))
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("datetimefunc" %in% search()) detach("datetimefunc")
attach(datetimefunc)  # subsequent additions not found, so attach at the end
