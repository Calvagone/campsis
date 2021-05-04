
#' Get random seed value.
#' 
#' @return random seed value generated based on time
#' @keywords internal
getRandomSeedValue <- function() {
  seed <- as.numeric(Sys.time())
  return(seed)
}

#' Get seed value.
#' 
#' @param seed user-input seed, NULL it not specified
#' @return a seed value
#' @keywords internal
#' 
getSeed <- function(seed=NULL) {
  if (is.null(seed)) {
    retValue <- getRandomSeedValue()
  } else {
    retValue <- seed
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
  return(originalSeed + 31*replicate)
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