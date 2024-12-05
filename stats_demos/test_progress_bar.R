# test_progress_bar.R

library(progress)
library(tidyverse)

PROGRESS_BAR_STYLE <- 3  # for utils::txtProgressBar
PROGRESS_BAR_FORMAT <- paste0(
    "(:spin) [:bar] :percent ",
    "[:current/:total | Rate :tick_rate/s | ",
    "Elapsed :elapsedfull | Estimated time left :eta]"
)
# ... for progress::progress_bar.
# NB ":eta" sometimes includes a leading space, e.g. for "0s", but not
# always, e.g. for "14h". Its code involves progress::progress_bar() and
# prettyunits::vague_dt().
PROGRESS_BAR_CLEAR <- FALSE

SLEEP_TIME <- 0.2


mk_testdata <- function() {
    A <- "A"
    B <- "B"
    C <- "C"
    P <- "P"
    Q <- "Q"
    R <- "R"
    testdata <- tibble(expand.grid(g = c(A, B, C), p = c(P, Q, R)))
    testdata <- (
        rbind(testdata, testdata, testdata)
        %>% arrange(g, p)
        %>% mutate(
            x = 1:n(),
            y = 10 + x
        )
    )
    return(testdata)
}


test_progress_bar_within_group_modify_r_base <- function(testdata) {
    # BASIC

    cat(">>> Via utils::txtProgressBar (part of core R)\n")
    cat("    Shows for every tick.\n")
    ng <- dplyr::n_groups(testdata %>% group_by(g, p))
    cat("Number of groups:", ng, "\n")
    bar <- utils::txtProgressBar(
        min = 0,
        max = ng,
        style = PROGRESS_BAR_STYLE
    )
    gfunc <- function(x_data, y_key, group_id) {
        # - y_key contains the grouping variables
        # - x_data contains the rest

        # cat("y_key:\n"); print(y_key)
        # cat("x_data:\n"); print(x_data)

        # dplyr::cur_group_id() does not work here.
        # Nor does passing it in within the group_modify() call; it appears to
        # work but then fails when you try to use the value (deferred call?).
        this_group_id <- x_data$group_id[1]
        setTxtProgressBar(bar, this_group_id)
        # cat("tick\n")
        chunk <- tibble(
            group_id = x_data$group_id,
            x2 = x_data$x,
            y2 = x_data$y,
            z = x_data$x + x_data$y
        )
        Sys.sleep(SLEEP_TIME)
        return(chunk)
    }
    result <- (
        testdata
        %>% group_by(g, p)
        %>% mutate(
            group_id = dplyr::cur_group_id()
            # Can't use max(group_id) here; we're operating within each group.
            # Can't use dplyr::n_groups() here.
        )
        %>% group_modify(
            ~ gfunc(x_data = .x, y_key = .y)
            # Can't use dplyr::cur_group_id() here. Or, you can use it, but
            # when you try to extract its value, it gives an error.
        )
    )
    close(bar)
    return(result)
}


test_progress_bar_within_group_modify_progress <- function(testdata) {
    # BETTER, AND DOES AUTOMATIC TIME ESTIMATION.

    cat(">>> Via progress::progress_bar\n")
    cat("    Doesn't show for the first tick, so the first tick is #2\n")
    ng <- dplyr::n_groups(testdata %>% group_by(g, p))
    cat("Number of groups:", ng, "\n")
    bar <- progress::progress_bar$new(
        format = PROGRESS_BAR_FORMAT,
        total = ng,
        clear = PROGRESS_BAR_CLEAR    # If TRUE (default), clears the bar when finish
    )
    gfunc <- function(x_data, y_key, group_id) {
        bar$tick()
        # cat("tick\n")
        chunk <- tibble(
            x2 = x_data$x,
            y2 = x_data$y,
            z = x_data$x + x_data$y
        )
        Sys.sleep(SLEEP_TIME)
        return(chunk)
    }
    result <- (
        testdata
        %>% group_by(g, p)
        %>% group_modify(
            ~ gfunc(x_data = .x, y_key = .y)
            # Can't use dplyr::cur_group_id() here. Or, you can use it, but
            # when you try to extract its value, it gives an error.
        )
    )
    return(result)
}


testdata <- mk_testdata()
test_progress_bar_within_group_modify_r_base(testdata)
test_progress_bar_within_group_modify_progress(testdata)
