# misclang.R

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

misclang <- new.env()

#==============================================================================
# String manipulation
#==============================================================================

misclang$n_char_occurrences <- function(string, char)
{
    s2 <- gsub(char, "", string)
    return(nchar(string) - nchar(s2))
}

#==============================================================================
# Vector manipulation
#==============================================================================

misclang$vector_element_by_index_of_last_element <- function(x)
{
    x[ x[ length(x) ] ]
}

#==============================================================================
# Caching
#==============================================================================

misclang$load_or_run_function <- function(
        varname, filename, fn, ...,
        forcerun=FALSE,
        cache_filetype=c("rds", "rda"))
{

    # e.g. load_or_run_function("blibble", "mydata.Rda", mean, c(1,2,3))

    cache_filetype <- match.arg(cache_filetype)

    if (cache_filetype == "rda") {
        # .Rda, .Rdata
        if (!forcerun && file.exists(filename)) {
            cat("Loading", varname, "from file:", filename, "\n")
            load(filename)  # assumes it will load into a variable whose textual name is in varname
            cat("... loaded\n")
        } else {
            cat("Running function:", deparse(substitute(fn)), "\n")
            assign(varname, fn(...))
            cat("--- Saving", varname, "to file:", filename, "\n")
            save(list = c(varname), file=filename)
        }
        return(get(varname))

    } else {
        # .Rds; cleaner; saves only a single object but doesn't care about its name
        if (!forcerun && file.exists(filename)) {
            cat("Loading", varname, "from file:", filename, "\n")
            result <- readRDS(filename)
            cat("... loaded\n")
        } else {
            cat("Running function:", deparse(substitute(fn)), "\n")
            result <- fn(...)
            cat("--- Saving to file:", filename, "\n")
            saveRDS(result, file=filename)
        }
        return(result)

    }
}

#==============================================================================
# Factors
#==============================================================================

misclang$numeric_factor_to_numeric <- function(f)
{
    # http://stackoverflow.com/questions/3418128
    as.numeric(levels(f))[f]
}

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("misclang" %in% search()) detach("misclang")
attach(misclang)  # subsequent additions not found, so attach at the end
