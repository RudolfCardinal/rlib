# miscmath.R

# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

miscmath = new.env()

# =============================================================================
# Geometric mean
# =============================================================================

miscmath$geometric_mean <- function(x, na.rm=TRUE) {
    # geometric mean
    # http://stackoverflow.com/questions/2602583
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

# =============================================================================
# Harmonic mean
# =============================================================================

miscmath$harmonic_mean <- function(x) {
    1 / mean(1/x)
}

# =============================================================================
# Logarithmic sequence
# =============================================================================

miscmath$log_sequence <- function(pow10low, pow10high,
                                  minimum=NA, maximum=NA) {
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
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscmath" %in% search()) detach("miscmath")
attach(miscmath)  # subsequent additions not found, so attach at the end
