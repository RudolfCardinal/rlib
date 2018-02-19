# listassign.R

#==============================================================================
# List assignment, e.g. list[a, b] <- something
# This can't live in a sub-namespace, so we allow it to alter the global one.
#==============================================================================

# http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value/15140507#15140507

list <- structure(NA, class="result")

"[<-.result" <- function(x, ..., value) {
    # Examples based on: list[x,y,z] <- col2rgb("aquamarine")
    args <- as.list(match.call())  # e.g. list(`[<-.result`, x=`*tmp*`, x, y, z, value=<promise: 0xfcdb30>)
    args <- args[-c(1:2, length(args))]  # e.g. list(x, y, z)
    length(value) <- length(args)  # e.g. 3
    for (i in seq(along=args)) {
        a <- args[[i]]  # e.g. x (or y, or z)
        if (!missing(a)) {
            eval.parent(substitute(a <- v, list(a=a, v=value[[i]])))
        }
    }
    return(x)
}

# Test:
# list[x,y,z] <- col2rgb("aquamarine")


# Tried repeatedly to get this to work with <<-, but no joy:
# "object of type 'builtin' is not subsettable"

# ... see listfunc.R instead
