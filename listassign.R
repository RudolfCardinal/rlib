# listassign.R

#==============================================================================
# List assignment, e.g. list[a, b] <- something
# This can't live in a sub-namespace, so we allow it to alter the global one.
#==============================================================================

# http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value/15140507#15140507

list <- structure(NA, class="result")

"[<-.result" <- function(x, ..., value) {
    args <- as.list(match.call())
    args <- args[-c(1:2, length(args))]
    length(value) <- length(args)
    for (i in seq(along=args)) {
        a <- args[[i]]
        if (!missing(a)) {
            eval.parent(substitute(a <- v, list(a=a, v=value[[i]])))
        }
    }
    x
}

# Test:
# list[x,y,z] <- col2rgb("aquamarine")
