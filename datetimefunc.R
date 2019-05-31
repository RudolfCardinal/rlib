# datetimefunc.R

requireNamespace("lubridate")

#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

datetimefunc = new.env()


#==============================================================================
# Calculate age
#==============================================================================

datetimefunc$calendar_age_lubridate_1 <- function(dob, now) {
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

datetimefunc$calendarAge <- calendar_age_lubridate_1


calendar_age <- function(dob, now)
{
    # THIS IS BETTER.
    # https://stackoverflow.com/questions/31126726
    # https://stackoverflow.com/questions/3611314/calculate-ages-in-r
    from_lt <- as.POSIXlt(dob)
    to_lt <- as.POSIXlt(now)

    age <- to_lt$year - from_lt$year

    return(ifelse(
        to_lt$mon < from_lt$mon |
            (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
        age - 1,
        age
    ))
}


age_float_years <- function(dob, now, days_per_year = 365.25)
{
    days <- as.numeric(difftime(now, dob, units = "days"))
    return(days / days_per_year)
}


#==============================================================================
# Namespace-like method: http://stackoverflow.com/questions/1266279/#1319786
#==============================================================================

if ("datetimefunc" %in% search()) detach("datetimefunc")
attach(datetimefunc)  # subsequent additions not found, so attach at the end
