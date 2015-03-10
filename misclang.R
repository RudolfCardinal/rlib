# misclang.R

vector_element_by_index_of_last_element <- function(x) {
    x[ x[ length(x) ] ]
}

load_or_run_function <- function(varname, file, fn, ..., forcerun=FALSE) {

    # e.g. load_or_run_function("blibble", "mydata.Rda", mean, c(1,2,3))

    if (!forcerun && file.exists(file)) {
        cat("Loading", varname, "from file:", file, "\n")
        load(file) # assumes it will load into a variable whose textual name is in varname
    } else {
        cat("Running function:", deparse(substitute(fn)), "\n")
        assign(varname, fn(...))
        cat("--- Saving", varname, "to file:", file, "\n")
        save(list = c(varname), file=file)
    }
    return(get(varname))
}
