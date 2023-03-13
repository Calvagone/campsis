
#' Setup plan for the given simulation or hardware settings.
#' 
#' @param object simulation or hardware settings
#' @return nothing
#' @importFrom future multisession plan sequential tweak
#' @export
setupPlanDefault <- function(object) {
  if (is(object, "hardware_settings")) {
    hardware <- object
  } else if (is(object, "simulation_settings")) {
    hardware <- object@hardware
  } else {
    stop("object must be either simulation or hardware settings")
  }
  # Reset plan
  future::plan(future::sequential)
  
  # Prepare multi-threading simulation    
  if (hardware@cpu > 1) {
    # Replicate is at the first level of parallelisation
    if (hardware@replicate_parallel) {
      future::plan(future::multisession, workers=hardware@cpu)
    } else {
      future::plan(
        list(
          # This is because 'replicate_parallel' is disabled
          future::tweak(future::multisession, workers=1),
          # Workers are reserved for the second level of parallelisation
          future::tweak(future::multisession, workers=hardware@cpu)
        )
      )
    }
  }
}

#' Setup plan as sequential (i.e. no parallelisation).
#' 
#' @return nothing
#' @importFrom future plan sequential
#' @export
setupPlanSequential <- function() {
  # Use sequential
  future::plan(future::sequential)
}

#' Get scheduling mode for furrr (see argument 'scheduling' available in furrr options).
#' 
#' @param parallel use parallel computing with furrr, logical value
#' @return 1 for parallel computing, 0 otherwise
#' @keywords internal
getFurrrScheduling <- function(parallel) {
  return(ifelse(parallel, 1, 0))
}
