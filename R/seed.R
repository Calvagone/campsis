#' Get random seed value.
#'
#' @return random seed value generated based on time
#' @keywords internal
get_random_seed_value <- function() {
  seed <- as.integer(Sys.time())
  return(seed)
}

#' Get seed value.
#'
#' @param seed user-input seed. If NULL or NA, seed number will be random.
#' @return a seed value, integer
#' @keywords internal
#'
get_seed <- function(seed = NULL) {
  if (is.null(seed) || is.na(seed)) {
    retValue <- get_random_seed_value()
  } else {
    assertthat::assert_that(is.numeric(seed) && seed %% 1 == 0, msg = "seed is not integer")
    retValue <- as.integer(seed)
  }
  return(retValue)
}

#' Get seed for parameter uncertainty sampling.
#'
#' @param seed original seed
#' @return the seed value used to sample parameter uncertainty
#' @keywords internal
#'
get_seed_for_parameters_sampling <- function(seed) {
  return(seed - 1)
}

#' Get seed for dataset export.
#'
#' @param seed original seed
#' @param progress simulation progress
#' @return the seed value used to export the dataset
#' @keywords internal
#'
get_seed_for_dataset_export <- function(seed, progress) {
  return(as.integer(seed + (progress@replicate - 1) * (progress@iterations + 1)))
}

#' Get seed for iteration.
#'
#' @param seed original seed
#' @param progress simulation progress
#' @return the seed value to be used for the given replicate number and iteration
#' @keywords internal
#'
get_seed_for_iteration <- function(seed, progress) {
  return(get_seed_for_dataset_export(seed = seed, progress = progress) + progress@iteration)
}

#' Set the seed. The goal of this method is to centralize all calls to
#' the R method 'set.seed'.
#'
#' @param seed seed value, not NULL
#' @keywords internal
#'
set_seed <- function(seed) {
  assertthat::assert_that(is.numeric(seed), msg = "seed not numeric")
  set.seed(seed)
  #cat(paste0("SEED CHANGED TO: ", seed, "\n"))
}
