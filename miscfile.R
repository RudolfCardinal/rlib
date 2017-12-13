# miscfile.R

requireNamespace("data.table")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

miscfile = new.env()


#==============================================================================
# Directory functions
#==============================================================================

#' current script file (in full path)
#' @param
#' @return
#' @examples
#' works with Rscript, source() or in RStudio Run selection
#' @export

miscfile$current_script_file <- function()
{
    # https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
    # http://stackoverflow.com/a/32016824/2292993
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName))
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
                # Source'd via R console
                return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                if (rstudioapi::isAvailable()) {
                    # RStudio Run Selection
                    # http://stackoverflow.com/a/35842176/2292993
                    return(normalizePath(
                        rstudioapi::getActiveDocumentContext()$path))
                    # ... if RStudio is not running, will fail with
                    # "Error: RStudio not running"
                } else {
                    stop("Don't know how to find current script file; are ",
                         "you calling this function from the R command line?")
                }
            }
        }
    }
}

miscfile$current_script_directory <- function()
{
    csf <- current_script_file()
    return(dirname(csf))
}

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


miscfile$write_text <- function(filename, text)
{
    cat("Writing to ", filename, "...\n", sep="")
    f <- file(filename)
    writeLines(text, f)
    close(f)
    cat("... written\n")
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscfile" %in% search()) detach("miscfile")
attach(miscfile)  # subsequent additions not found, so attach at the end
