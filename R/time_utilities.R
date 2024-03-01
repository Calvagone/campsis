
#'
#' Convert seconds to hours.
#'
#' @param x numeric vector in seconds
#' @return numeric vector in hours
#' @export
seconds <- function(x) {
  return(x/(60*60))
}

#'
#' Convert minutes to hours.
#'
#' @param x numeric vector in minutes
#' @return numeric vector in hours
#' @export
minutes <- function(x) {
  return(x/60)
}

#'
#' Convert hours to hours (do nothing).
#'
#' @param x numeric vector in hours
#' @return numeric vector in hours
#' @export
hours <- function(x) {
  return(x)
}

#'
#' Convert days to hours.
#'
#' @param x numeric vector in days
#' @return numeric vector in hours
#' @export
days <- function(x) {
  return(x*24)
}

#'
#' Convert weeks to hours.
#'
#' @param x numeric vector in weeks
#' @return numeric vector in hours
#' @export
weeks <- function(x) {
  return(x*7*24)
}

#'
#' Convert pharma months (1 month = 4 weeks) to hours.
#'
#' @param x numeric vector in months
#' @return numeric vector in hours
#' @export
months <- function(x) {
  return(x*4*7*24)
}

#'
#' Convert pharma years (1 year = 12*4 weeks) to hours.
#'
#' @param x numeric vector in years
#' @return numeric vector in hours
#' @export
years <- function(x) {
  return(x*12*4*7*24)
}

#'
#' Return the list of available time units.
#'
#' @return character vector
#' @export
getAvailableTimeUnits <- function() {
  return(c("second", "minute", "hour", "day", "week", "month", "year"))
}

#'
#' Convert numeric time vector based on the provided units.
#' 
#' @param x numeric time vector
#' @param from unit of x, single character value
#' @param to destination unit, single character value
#' @return numeric vector with the converted times
#' @export
convertTime <- function(x, from, to) {
  assertthat::assert_that(is.numeric(x), msg="x is not numeric")
  from_ <- standardiseTime(x=1, unit=from)
  to_ <- standardiseTime(x=1, unit=to)
  return(x*from_/to_)
}

#'
#' Standardise time to hours.
#' 
#' @param x numeric time vector
#' @param unit unit of x, single character value
#' @return numeric vector with the times converted to hours
#' @export
standardiseTime <- function(x, unit) {
  assertthat::assert_that(length(unit)==1, msg="argument 'unit' must be length 1")
  assertthat::assert_that(unit %in% getAvailableTimeUnits(),
                          msg=sprintf("argument 'unit' is incorrect, unit must be one of: %s",
                                      paste0(getAvailableTimeUnits(), collapse=", ")))
  if (unit=="second") {
    return(seconds(x))
    
  } else if (unit=="minute") {
    return(minutes(x))
    
  } else if (unit=="hour") {
    return(hours(x))
    
  } else if (unit=="day") {
    return(days(x))
    
  } else if (unit=="week") {
    return(weeks(x))
    
  } else if (unit=="month") {
    return(months(x))
    
  } else if (unit=="year") {
    return(years(x))
    
  } else {
    stop("Should never occur")
  }
}
