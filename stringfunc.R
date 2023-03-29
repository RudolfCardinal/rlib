#!/usr/bin/env Rscript

tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    gsubfn
)
rm(tmp_require_package_namespace)


# =============================================================================
# Top-level namespace: %format%
# =============================================================================

# https://stackoverflow.com/questions/17475803/sprintf-format-strings-reference-by-name

`%format%` <- function(fmt, list) {
    pat <- "%\\(([^)]*)\\)"
    fmt2 <- gsub(pat, "%", fmt)
    list2 <- list[strapplyc(fmt, pat)[[1]]]
    do.call("sprintf", c(fmt2, list2))
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

stringfunc <- new.env()


stringfunc$formatString <- function(fmt, list) {
    return(with(list, gsubfn::fn$identity(fmt)))
}


stringfunc$TESTS <- '

stringfunc$formatString("some $text $to $replace", list(
    text = "day",
    to = "in",
    replace = "June"
))

'
