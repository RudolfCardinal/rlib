# miscmath.R

# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

miscmath <- new.env()


# =============================================================================
# Geometric mean
# =============================================================================

miscmath$geometric_mean <- function(x, na.rm = TRUE) {
    # geometric mean
    # http://stackoverflow.com/questions/2602583
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}


# =============================================================================
# Harmonic mean
# =============================================================================

miscmath$harmonic_mean <- function(x) {
    1 / mean(1/x)
}


# =============================================================================
# Logistic function
# =============================================================================

miscmath$logistic <- function(x, x0, k, L = 1) {
    # Notation as per https://en.wikipedia.org/wiki/Logistic_function
    L / (1 + exp(-k * (x - x0)))
}


# =============================================================================
# Logarithmic sequence
# =============================================================================

miscmath$log_sequence <- function(pow10low, pow10high,
                                  minimum = NA, maximum = NA) {
    # Breaks for a logithmic graph axis -- for example:
    #       1, 2, 3, ..., 9, 10, 20, 30, ..., 90, 100, 200, 300, ..., 1000
    # http://stackoverflow.com/questions/23901907
    x <- c(2:10 %o% 10^(pow10low:pow10high))
    if (!is.na(minimum)) {
        x <- x[which(x >= minimum)]
    }
    if (!is.na(maximum)) {
        x <- x[which(x <= maximum)]
    }
    return(x)
}


# =============================================================================
# Probability
# =============================================================================

miscmath$odds_from_probability <- function(p) {
    # Returns odds, given a probability.
    return(p / (1 - p))
}


miscmath$ln_odds_from_probability <- function(p) {
    # Returns (natural) log odds, given a probability.
    return(log(miscmath$odds_from_probability(p)))
}


miscmath$probability_from_odds <- function(odds) {
    # Returns probability, given odds.
    return(odds / (1 + odds))
    # Since o = p / (1 - p), 
    # therefore (1 + o) = (1 - p) / (1 - p) + p / (1 - p) = 1 / (1 - p)
    # and from that, o / (1 + o) = p.
}


miscmath$probability_from_log_odds <- function(log_odds) {
    # Returns probability, given (natural) log odds.
    return(miscmath$probability_from_odds(exp(log_odds)))
}


# =============================================================================
# Formatting numbers
# =============================================================================

miscmath$is_integer <- function(x) {
    # http://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
    x %% 1 == 0
}


miscmath$format_dp <- function(x, dp) {
    # http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
    format(round(x, dp), nsmall = dp)
}


miscmath$format_dp_unless_integer <- function(x, dp) {
    # http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
    ifelse(
        miscmath$is_integer(x),
        as.character(x),
        format_dp(x, dp)
    )
}


miscmath$format_sf <- function(x, sf = 3,
                               scientific = FALSE,
                               big.mark = ",", big.interval = 3,
                               small.mark = "", small.interval = 3,
                               drop0trailing = TRUE) {
    # http://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
    format(signif(x, sf),
           scientific = scientific,
           big.mark = big.mark,
           big.interval = big.interval,
           small.mark = small.mark,
           small.interval = small.interval,
           drop0trailing = drop0trailing)
}


miscmath$describe_p_value <- function(p, boundary_NS = 0.1,
                                      boundary_scientific = 0.0001) {
    ifelse(
        p > boundary_NS,
        paste("p >", boundary_NS),
        ifelse(
            p == 0,  # unusual!
            "p = 0",  # better than "p = 0e0+00"
            ifelse(
                p >= boundary_scientific,
                paste("p =", miscmath$format_sf(p, scientific = FALSE,
                                                small.mark = "")),
                paste("p =", miscmath$format_sf(p, scientific = TRUE))
            )
        )
    )
}


miscmath$p_stars <- function(p) {
    # Simply a convention!
    # http://stats.stackexchange.com/questions/29158/do-you-reject-the-null-hypothesis-when-p-alpha-or-p-leq-alpha
    ifelse(p <= 0.001, "***",
           ifelse(p <= 0.01, "**",
                  ifelse(p < 0.05, "*",
                         "NS")))
}


miscmath$describe_p_value_with_stars <- function(p) {
    paste(miscmath$p_stars(p), ", ", miscmath$describe_p_value(p), sep = "")
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscmath" %in% search()) detach("miscmath")
attach(miscmath)  # subsequent additions not found, so attach at the end
