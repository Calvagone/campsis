
toExplicitDistribution <- function(distribution) {
  if (is.null(distribution)) {
    return(new("undefined_distribution"))
  } else if (is.numeric(distribution)) {
    if (distribution %>% length() > 1) {
      stop("Distribution has more than 1 value")
    }
    return(ConstantDistribution(distribution))
  } else if (is(distribution, "distribution")) {
    return(distribution)
  } else {
    stop("Not a distribution nor a numeric value")
  }
}