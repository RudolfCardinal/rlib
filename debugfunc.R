# debugfunc.R

library(xtermStyle)

#==============================================================================
# Debug message
#==============================================================================

debug_message <- function(..., file="", filename="", append=TRUE) {
    # Use the file parameter OR the filename parameter OR neither.
    mode = "console"
    if (file != "") {
        mode = "file"
    } else if (filename != "") {
        mode = "filename"
    }

    if (mode == "file") {
        sink(file, append=append)
    } else if (mode == "filename") {
        sink(filename, append=append)
    }

    cat(..., "\n", sep="")

    if (mode == "file" || mode == "filename") {
        sink()
    }
}

#==============================================================================
# Debug a thing
#==============================================================================

debug_quantity <- function(x, file="", filename="", append=TRUE,
                           progress_to_console=TRUE) {
    # Use the file parameter OR the filename parameter OR neither.

    # To write to file, can use:
    #       cat(..., file=file)
    #       dput(x, file=file)
    # but not:
    #       print(x)
    # So the simplest way is to use sink().
    # This maintains a stack of diversions, so we divert then un-divert
    # Note also:
    #       f <- open("output.txt", open="w")  # w=write, a=append
    #       # write stuff
    #       flush(f)  # if desired
    #       close(f)

    mode = "console"
    if (file != "") {
        mode = "file"
    } else if (filename != "") {
        mode = "filename"
    }

    x_name <- deparse(substitute(x))  # fetch the variable name passed in
    LINEBREAK <- paste(c(rep("-", 79), "\n"), collapse="")

    if (progress_to_console) {
        destination = mode
        if (mode == "filename") {
            destination = filename
        }
        cat("DEBUGGING QUANTITY: ", x_name, " TO ", destination, "\n", sep="")
    }

    if (mode == "file") {
        sink(file, append=append)
    } else if (mode == "filename") {
        sink(filename, append=append)
    }

    cat(LINEBREAK, "DEBUGGING QUANTITY: ", x_name, "\n", sep="")
    cat("... print(", x_name, "):\n", sep="")
    print(x)
    cat("... dput(", x_name, "):\n", sep="")
    dput(x, file=file)
    cat(LINEBREAK, "\n", file=file, sep="")

    if (mode == "file" || mode == "filename") {
        sink()
    }
}

#f <- file("output.txt", open="w")
#x <- list(a=1, b=2)
#debug_quantity(x, file=f)
#close(f)


#==============================================================================
# Output columns
#==============================================================================

wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

#==============================================================================
# Nested status messages
#==============================================================================

STATUS_DISPLAY_LEVEL <- 0

status <- function(msg, ellipsis=FALSE, increment=FALSE, colour="blue") {
    cat(
        style(
            paste(
                paste(rep("  ", STATUS_DISPLAY_LEVEL), collapse=""),
                msg,
                ifelse(ellipsis, "...", ""),
                "\n",
                sep=""
            ),
            fg=colour
        )
    )
    if (increment) {
        STATUS_DISPLAY_LEVEL <<- STATUS_DISPLAY_LEVEL + 1
    }
}

status_start <- function(msg, ellipsis=TRUE, colour="red") {
    status(msg, increment=TRUE, ellipsis=ellipsis, colour=colour)
}

status_end <- function(msg="... done", announce=TRUE, colour="green") {
    STATUS_DISPLAY_LEVEL <<- STATUS_DISPLAY_LEVEL - 1
    if (announce) {
        cat(
            style(
                paste(
                    paste(rep("  ", STATUS_DISPLAY_LEVEL), collapse=""),
                    msg,
                    "\n",
                    sep=""
                ),
                fg=colour
            )
        )
    }
}

