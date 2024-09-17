#!/usr/bin/env Rscript

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        data.table,
        RODBC
    )
})


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

dbfunc <- new.env()


# =============================================================================
# Database connections and queries
# =============================================================================

dbfunc$connectCpft <- function(
    server = "CPFT-CRATE-P01",
    port = 1433,
    # odbc_driver = "SQL Server",
    odbc_driver = "ODBC Driver 11 for SQL Server")
{
    # Uses Trusted_Connection=Yes, i.e. Windows authentication.
    connection_str <- paste0(
        'driver={', odbc_driver, '}',
        ';server=', server, ',', port,
        ';Trusted_Connection=Yes'
    )
    cat("Connecting to: ", connection_str, "\n", sep = "")
    return(RODBC::odbcDriverConnect(connection_str))
}


dbfunc$sqlQuery <- function(dbhandle, sql, debug = TRUE, errors = TRUE) {
    if (debug) {
        cat("Executing: ", sql, "\n", sep = "")
    }
    result <- RODBC::sqlQuery(dbhandle, sql, errors = errors)
    if (is.data.frame(result)) {
        # success
        return(data.table(result))
    }
    print(result)  # errors
    stop(sprintf(
        "Aborting because of error in SQL execution; first error was: %s",
        result[1]))
}


dbfunc$getCachedQuery <- function(filename, dbhandle, sql)
{
    if (file.exists(filename)) {
        cat(sprintf("Loading cached query from %s\n", filename))
        return(dbfunc$readRds(filename))
    }
    result <- dbfunc$sqlQuery(dbhandle, sql)
    dbfunc$writeRds(result, filename)
    return(result)
}


dbfunc$writeRds <- function(d, filename) {
    cat(sprintf("Writing to %s\n", filename))
    saveRDS(d, filename)
}


dbfunc$readRds <- function(filename) {
    cat(sprintf("Reading from %s\n", filename))
    x <- readRDS(filename)
    if (is.data.table(x)) {
        alloc.col(x)
        # ... https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-faq.html#reading-data.table-from-rds-or-rdata-file
        # ... http://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
    }
    return(x)
}
