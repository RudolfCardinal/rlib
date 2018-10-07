# datetimefunc.R

requireNamespace("lubridate")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

datetimefunc = new.env()


#==============================================================================
# Calculate age
#==============================================================================

datetimefunc$calendarAge <- function(dob, now) {
    # Works with POSIXct or Date values
    # http://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate
    interval_period <- lubridate::interval(dob, now)
    full_years <- interval_period %/% lubridate::years(1)
    # remaining_weeks <- (
    #     interval_period %% lubridate::years(1) %/% lubridate::weeks(1)
    # )
    # remaining_days <- (
    #     interval_period 
    #     %% lubridate::years(1) 
    #     %% lubridate::weeks(1)
    #     %/% lubridate::days(1)
    # )
    return(full_years)
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("datetimefunc" %in% search()) detach("datetimefunc")
attach(datetimefunc)  # subsequent additions not found, so attach at the end
