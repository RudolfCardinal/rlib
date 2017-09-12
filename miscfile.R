# miscfile.R

requireNamespace("data.table")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscfile = new.env()


#==============================================================================
# File functions
#==============================================================================

miscfile$write_tsv <- function(DT, filename, quote=TRUE)
{
    write.table(DT, file=filename, quote=quote, sep='\t', col.names=TRUE,
                row.names=FALSE)
}


miscfile$write_rds <- function(d, filename)
{
    cat(sprintf("Writing to %s\n", filename))
    saveRDS(d, filename)
}


miscfile$read_rds <- function(filename)
{
    cat(sprintf("Reading from %s\n", filename))
    x <- readRDS(filename)
    if (is.data.table(x)) {
        alloc.col(x)
        # ... https://rawgit.com/wiki/Rdatatable/data.table/vignettes/datatable-faq.html#reading-data.table-from-rds-or-rdata-file
        # ... http://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
    }
    return(x)
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscfile" %in% search()) detach("miscfile")
attach(miscfile)  # subsequent additions not found, so attach at the end
