
#' Get random seed value.
#' 
#' @return random seed value generated based on time
#' @keywords internal
getRandomSeedValue <- function() {
  seed <- as.integer(Sys.time())
  return(seed)
}

#' Get seed value.
#' 
#' @param seed user-input seed, NULL it not specified
#' @return a seed value, integer
#' @keywords internal
#' 
getSeed <- function(seed=NULL) {
  if (is.null(seed)) {
    retValue <- getRandomSeedValue()
  } else {
    assertthat::assert_that(is.numeric(seed) && seed%%1==0, msg="seed is not integer")
    retValue <- as.integer(seed)
  }
  return(retValue)
}

#' Get seed for parameter uncertainty sampling.
#' 
#' @param seed original seed
#' @return the seed value used to sample parameter uncertainty
#' @export
#' 
getSeedForParametersSampling <- function(seed) {
  return(seed - 1)
}

#' Get seed for dataset export.
#' 
#' @param seed original seed
#' @param replicate the current replicate number
#' @param iterations total number of iterations
#' @return the seed value used to export the dataset
#' @export
#' 
getSeedForDatasetExport <- function(seed, replicate, iterations) {
  return(as.integer(seed + (replicate - 1)*(iterations + 1)))
}

#' Get seed for iteration.
#' 
#' @param seed original seed
#' @param replicate the current replicate number
#' @param iterations total number of iterations
#' @param iteration current iteration number
#' @return the seed value to be used for the given replicate number and iteration
#' @export
#' 
getSeedForIteration <- function(seed, replicate, iterations, iteration) {
  return(getSeedForDatasetExport(seed, replicate, iterations) + iteration)
}

#' Set the seed. The goal of this method is to centralise all calls to
#' the R method 'set.seed'.
#' 
#' @param seed seed value, not NULL
#' @keywords internal
#' 
setSeed <- function(seed) {
  assertthat::assert_that(is.numeric(seed), msg="seed not numeric")
  set.seed(seed)
  #cat(paste0("SEED CHANGED TO: ", seed, "\n"))
}