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
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("miscmath" %in% search()) detach("miscmath")
attach(miscmath)  # subsequent additions not found, so attach at the end
