# miscmath.R

#==============================================================================
# Geometric mean
#==============================================================================

geometric_mean <- function(x, na.rm=TRUE) {
    # geometric mean
    # http://stackoverflow.com/questions/2602583
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
