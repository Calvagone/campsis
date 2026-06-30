#' Compute the prediction interval summary over time (deprecated).
#' 
#' @param x data frame
#' @param variable variable(s) used to compute the prediction interval, character vector.
#'   When more than one variable is supplied, a \code{variable} column is added to the
#'   output to identify each one.
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @return a summary table
#' @export
#' @keywords internal
PI <- function(x, variable, strata = getDefaultStrata(), level = 0.90) {
  .Deprecated("compute_pi")
  return(compute_pi(x=x, variable=variable, strata=strata, level=level))  
}

