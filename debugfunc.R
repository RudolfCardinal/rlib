# debugfunc.R

tmp_require_package_namespace <- function(...) {
    packages <- as.character(match.call(expand.dots = FALSE)[[2]])
    for (p in packages) if (!requireNamespace(p)) install.packages(p)
}
tmp_require_package_namespace(
    xtermStyle
)
rm(tmp_require_package_namespace)


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

debugfunc <- new.env()


# =============================================================================
# Debug message
# =============================================================================

debugfunc$debug_message <- function(..., file = "", filename = "", append = TRUE) {
    # Use the file parameter OR the filename parameter OR neither.
    mode <- "console"
    if (file != "") {
        mode <- "file"
    } else if (filename != "") {
        mode <- "filename"
    }

    if (mode == "file") {
        sink(file, append = append)
    } else if (mode == "filename") {
        sink(filename, append = append)
    }

    cat(..., "\n", sep = "")

    if (mode == "file" || mode == "filename") {
        sink()
    }
}


# =============================================================================
# Debug a thing
# =============================================================================

debugfunc$debug_quantity <- function(
        x, file = "", filename = "", append = TRUE,
        progress_to_console = TRUE, print_only = FALSE
) {
    # Use the file parameter OR the filename parameter OR neither.

    # To write to file, can use:
    #       cat(..., file = file)
    #       dput(x, file = file)
    # but not:
    #       print(x)
    # So the simplest way is to use sink().
    # This maintains a stack of diversions, so we divert then un-divert
    # Note also:
    #       f <- open("output.txt", open = "w")  # w = write, a = append
    #       # write stuff
    #       flush(f)  # if desired
    #       close(f)

    mode <- "console"
    if (file != "") {
        mode <- "file"
    } else if (filename != "") {
        mode <- "filename"
    }

    x_name <- deparse(substitute(x))  # fetch the variable name passed in
    LINEBREAK_1 <- paste(c(rep("=", 79), "\n"), collapse = "")
    LINEBREAK_2 <- paste(c(rep("-", 79), "\n"), collapse = "")

    if (progress_to_console) {
        destination <- mode
        if (mode == "filename") {
            destination <- filename
        }
        cat("DEBUGGING QUANTITY: ", x_name, " TO ", destination, "\n", sep = "")
    }

    if (mode == "file") {
        sink(file, append = append)
    } else if (mode == "filename") {
        sink(filename, append = append)
    }

    cat(LINEBREAK_1, "DEBUGGING QUANTITY: ", x_name, "\n", sep = "")
    cat(LINEBREAK_2, "... print(", x_name, "):\n", LINEBREAK_2, sep = "")
    print(x)
    if (!print_only) {
        cat(LINEBREAK_2, "... dput(", x_name, "):\n", LINEBREAK_2, sep = "")
        dput(x, file = file)
    }
    cat(LINEBREAK_1, file = file, sep = "")

    if (mode == "file" || mode == "filename") {
        sink()
    }
}
#f <- file("output.txt", open = "w")
#x <- list(a = 1, b = 2)
#debug_quantity(x, file = f)
#close(f)


debugfunc$wtf_is <- function(x) {
    # For when you have no idea what something is.
    # http://stackoverflow.com/questions/8855589
    cat("1. typeof():\n")
    print(typeof(x))
    cat("\n2. class():\n")
    print(class(x))
    cat("\n3. mode():\n")
    print(mode(x))
    cat("\n4. names() [try x$somename, x[\"somename\"], x[[\"somename\"]] ]:\n")
    print(names(x))
    cat("\n5. slotNames() [use x@someslot or slot(x, \"someslot\")]:\n")
    print(slotNames(x))
    cat("\n6. attributes() [use attr(x, \"someattr\")]:\n")
    print(attributes(x))
    cat("\n7. str():\n")
    print(str(x))
    cat("\n8. methods, for each class type:\n")
    for (c in class(x)) {
        cat(paste('\n- methods(class = "', c, '"):\n', sep = ""))
        print(methods(class = c))
    }
}


# =============================================================================
# Output columns
# =============================================================================

debugfunc$wideScreen <- function(howWide = Sys.getenv("COLUMNS")) {
    if (nchar(howWide) > 0) {  # under non-console clusters, envvar can be empty
        options(width = as.integer(howWide))
    }
}


# =============================================================================
# Nested status messages
# =============================================================================

debugfunc$STATUS_DISPLAY_LEVEL <- 0


debugfunc$status <- function(msg, ellipsis = FALSE, increment = FALSE, colour = "blue") {
    cat(
        style(
            paste(
                paste(rep("  ", debugfunc$STATUS_DISPLAY_LEVEL), collapse = ""),
                msg,
                ifelse(ellipsis, "...", ""),
                "\n",
                sep = ""
            ),
            fg = colour
        )
    )
    if (increment) {
        debugfunc$STATUS_DISPLAY_LEVEL <<- debugfunc$STATUS_DISPLAY_LEVEL + 1
    }
}


debugfunc$status_start <- function(msg, ellipsis = TRUE, colour = "red") {
    status(msg, increment = TRUE, ellipsis = ellipsis, colour = colour)
}


debugfunc$status_end <- function(msg = "... done", announce = TRUE, colour = "green") {
    debugfunc$STATUS_DISPLAY_LEVEL <<- debugfunc$STATUS_DISPLAY_LEVEL - 1
    if (announce) {
        cat(style(
            paste(
                paste(rep("  ", debugfunc$STATUS_DISPLAY_LEVEL), collapse = ""),
                msg,
                "\n",
                sep = ""
            ),
            fg = colour))
    }
}


# =============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
# =============================================================================

if ("debugfunc" %in% search()) detach("debugfunc")
attach(debugfunc)  # subsequent additions not found, so attach at the end
