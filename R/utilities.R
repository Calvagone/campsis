
#' 
#' Convert user-given distribution to an explicit pmxsim distribution.
#' Passed distribution can be:
#' - a NULL value. In that case, it will be converted into is a 'UndefinedDistribution'.
#' - a single numeric value. In that case, it will be converted into is a 'ConstantDistribution'.
#' - a numeric vector. In that case, it will be converted into is a 'FixedDistribution'.
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