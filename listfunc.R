# listfunc.R

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

listfunc = new.env()


#==============================================================================
# Assign list to variables in global environment
#==============================================================================

listfunc$list_assign <- function(names, values, global=FALSE)
{
    if (length(names) != length(values)) {
        stop("length(names) != length(values)")
    }
    for (i in seq(along.with=names)) {
        assign(x=names[[i]],
               value=values[[i]],
               envir=parent.frame(),
               inherits=global)  # if TRUE, behaviour matches "<<-"; see ?"<<-" and ?assign
    }
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("listfunc" %in% search()) detach("listfunc")
attach(listfunc)  # subsequent additions not found, so attach at the end


# Test with:
# listfunc$list_assign(c("x", "y", "z"), col2rgb("aquamarine"), global=TRUE)
