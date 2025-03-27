
#' 
#' Import the whole campsismod package into NAMESPACE when parsed by 'roxygen'.
#' 
#' @import campsismod
#' @keywords internal
#' @return always TRUE
#' 
importCampsismodToNamespace <- function() {
  return(TRUE)
}

#' 
#' Convert user-given distribution to an explicit CAMPSIS distribution.
#' Passed distribution can be:
#' - a NULL value. In that case, it will be converted into an 'UndefinedDistribution'.
#' - a single numeric value. In that case, it will be converted into a 'ConstantDistribution'.
#' - a numeric vector. In that case, it will be converted into a 'FixedDistribution'.
#' - all available types of distribution. In this case, no conversion is applied.
#' 
#' @param distribution user-given distribution
#' @return a distribution object
#' @keywords internal
#' 
toExplicitDistribution <- function(distribution) {
  if (is.null(distribution)) {
    return(new("undefined_distribution"))
  } else if (is.numeric(distribution)) {
    if (distribution %>% length() > 1) {
      return(FixedDistribution(distribution))
    } else {
      return(ConstantDistribution(distribution))
    }
  } else if (is(distribution, "distribution")) {
    return(distribution)
  } else {
    stop("Not a distribution nor a numeric value")
  }
}

toExplicitDistributionList <- function(distribution, cmtNo) {
  if (is.null(distribution)) {
    retValue <- list(new("undefined_distribution"))
  }
  if (is.numeric(distribution)) {
    # E.g. f=c(0.5, 1)
    retValue <- distribution %>% purrr::map(toExplicitDistribution)
    
  } else if (is.list(distribution)) {
    # E.g. f=list(0.5, 1)
    retValue <- distribution %>% purrr::map(toExplicitDistribution)
  } else {
    retValue <- list(toExplicitDistribution(distribution))
  }
  size <- length(retValue)
  if (size==cmtNo) {
    return(retValue)
  } else if (size==1 && cmtNo > 1) {
    return(rep(retValue, cmtNo))
  } else {
    stop("Invalid distribution")
  }
}

envVarIsTrue <- function(x) {
  return(isTRUE(as.logical(Sys.getenv(x, "false"))))
}

#'
#' Check if the current session is on CRAN. The objective is to potentially suppress 
#' long tasks to be run on CRAN (long tests or vignettes).
#'
#' @return logical value TRUE/FALSE
#' @export
#' @keywords internal
onCran <- function() {
  # Copied from testthat:::on_cran() 
  return(!interactive() && !envVarIsTrue("NOT_CRAN"))
}

#'
#' Check if the current session is on CI (e.g. GitHub actions).
#'
#' @return logical value TRUE/FALSE
#' @export
#' @keywords internal
onCI <- function() {
  # Copied from testthat:::on_ci() 
  return(envVarIsTrue("CI"))
}
