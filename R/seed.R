
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

#' Get seed for replicate.
#' 
#' @param originalSeed original seed
#' @param replicate the replicate number
#' @return a new seed value for this replicate
#' @keywords internal
#' 
getSeedForReplicate <- function(originalSeed, replicate) {
  return(as.integer(originalSeed + 31*(replicate - 1)))
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
}