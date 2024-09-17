# miscfile.R

local({
    tmp_require_package_namespace <- function(...) {
        packages <- as.character(match.call(expand.dots = FALSE)[[2]])
        for (p in packages) if (!requireNamespace(p)) install.packages(p)
    }
    tmp_require_package_namespace(
        data.table,
        openxlsx
    )
})


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

miscfile <- new.env()


# =============================================================================
# Directory functions
# =============================================================================

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
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars <- ls(sys.frames()[[1]])
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


# =============================================================================
# File functions: TSV, RDS
# =============================================================================

miscfile$write_tsv <- function(DT, filename, quote = TRUE)
{
    write.table(DT, file = filename, quote = quote, sep = '\t', col.names = TRUE,
                row.names = FALSE)
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
    cat("Writing to ", filename, "...\n", sep = "")
    f <- file(filename)
    writeLines(text, f)
    close(f)
    cat("... written\n")
}


# =============================================================================
# Spreadsheet output
# =============================================================================

write_sheet <- function(data, filename, sheet_name, append = TRUE) {
    # Write a sheet (table) to an XLSX spreadsheet file.
    #
    # https://stackoverflow.com/questions/27713310
    # https://stackoverflow.com/questions/28053185

    cat(
        "... writing sheet ", sheet_name, " to ", filename, "\n",
        sep = ""
    )
    if (FALSE) {
        # ---------------------------------------------------------------------
        # xlsx, plain attempt
        # ---------------------------------------------------------------------
        # This will fail if the sheet already exists:
        # Error in .jcall(wb, "Lorg/apache/poi/ss/usermodel/Sheet;", "createSheet", :
        #  java.lang.IllegalArgumentException: The workbook already contains a sheet of this name

        xlsx::write.xlsx2(
            data,
            file = filename,
            sheetName = sheet_name,
            append = append,
            row.names = FALSE
        )
    } else if (FALSE) {
        # ---------------------------------------------------------------------
        # xlsx, straightforward remove-if-exists
        # ---------------------------------------------------------------------
        # No better -- raises:
        #
        # Error in .jcall(wb, "Lorg/apache/poi/ss/usermodel/Sheet;", "createSheet", :
        #  java.lang.IllegalArgumentException: Sheet index (-1) is out of range (0..0)

        if (file.exists(filename) && append) {
            wb <- xlsx::loadWorkbook(filename)
        } else {
            wb <- xlsx::createWorkbook()
        }
        # sheets <- xlsx::getSheets(wb)  # makes no difference
        xlsx::removeSheet(wb, sheetName = sheet_name)
        new_sheet <- xlsx::createSheet(wb, sheetName = sheet_name)
        xlsx::addDataFrame(data, new_sheet)
        xlsx::saveWorkbook(wb, filename)
    } else if (FALSE) {
        # ---------------------------------------------------------------------
        # xlsx, remove-save-reload if exists
        # ---------------------------------------------------------------------
        # This is ugly, but works! I think that removing a sheet doesn't
        # properly clear the record of sheets, so a subsequent addition
        # fails unless you save/reload.
        # However, it falls over with Java memory errors fairly easily.

        maxlen <- 31  # Determined empirically! See below.
        if (stringr::str_length(sheet_name) > maxlen) {
            # If you don't do this, you get "sheet already exists" errors.
            # I think the bug is probably:
            # - xlsx: "Do you have this name?
            #   dx_hyperparathyroid_or_hypercalcaemia"
            # - Spreadsheet: "No."
            # - xlsx: "Insert as dx_hyperparathyroid_or_hypercalcaemia."
            # - Spreadsheet: "I'll store it as
            #   dx_hyperparathyroid_or_hypercal. Oh, that one does already
            #   exist. Crash."
            tmp_shorter <- stringr::str_sub(sheet_name, 1, maxlen)
            cat(
                "Shortening spreadsheet name ", sheet_name, " to ",
                tmp_shorter, "\n", sep = ""
            )
            sheet_name <- tmp_shorter
        }
        if (file.exists(filename) && append) {
            wb <- xlsx::loadWorkbook(filename)
            sheets <- xlsx::getSheets(wb)
            if (!is.null(sheets[[sheet_name]])) {
                # Sheet already exists
                xlsx::removeSheet(wb, sheetName = sheet_name)
                xlsx::saveWorkbook(wb, filename)
                wb <- xlsx::loadWorkbook(filename)
            }
        } else {
            wb <- xlsx::createWorkbook()
        }
        new_sheet <- xlsx::createSheet(wb, sheetName = sheet_name)
        xlsx::addDataFrame(data, new_sheet)
        xlsx::saveWorkbook(wb, filename)
    } else if (TRUE) {
        # ---------------------------------------------------------------------
        # openxlsx, straightforward remove-if-exists
        # ---------------------------------------------------------------------

        maxlen <- 31  # Not checked for openxlsx, but just for safety...
        if (stringr::str_length(sheet_name) > maxlen) {
            tmp_shorter <- stringr::str_sub(sheet_name, 1, maxlen)
            cat(
                "Shortening spreadsheet name ", sheet_name, " to ",
                tmp_shorter, "\n", sep = ""
            )
            sheet_name <- tmp_shorter
        }
        if (file.exists(filename) && append) {
            wb <- openxlsx::loadWorkbook(filename)
            existing_sheet_names <- openxlsx::getSheetNames(filename)
            # ... operates from filename, not workbook object
            if (sheet_name %in% existing_sheet_names) {
                # Sheet already exists
                openxlsx::removeWorksheet(wb, sheet = sheet_name)
            }
        } else {
            wb <- openxlsx::createWorkbook()
        }
        openxlsx::addWorksheet(wb, sheetName = sheet_name)
        openxlsx::writeData(wb = wb, sheet = sheet_name, x = data)
        openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    }
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("miscfile" %in% search()) detach("miscfile")
attach(miscfile)  # subsequent additions not found, so attach at the end
