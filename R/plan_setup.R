
#' Setup default plan for the given simulation or hardware settings.
#' This plan will prioritise the distribution of workers in the following order:
#' 1) Replicates (if 'replicate_parallel' is enabled)
#' 2) Scenarios (if 'scenario_parallel' is enabled)
#' 3) Dataset export / slices (if 'dataset_export' or 'slice_parallel' is enabled)  
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
    if (hardware@replicate_parallel) {
      future::plan(future::multisession, workers=hardware@cpu)
    } else {
      if (hardware@scenario_parallel) {
        future::plan(
          list(
            future::tweak(future::multisession, workers=1),           # Replicates' level
            future::tweak(future::multisession, workers=hardware@cpu) # Scenarios' level
          )
        )
      } else {
        if (hardware@dataset_parallel || hardware@slice_parallel) {
          future::plan(
            list(
              future::tweak(future::multisession, workers=1),           # Replicates' level
              future::tweak(future::multisession, workers=1),           # Scenarios' level
              future::tweak(future::multisession, workers=hardware@cpu) # Slices' level
            )
          )
        } else {
          # Do nothing
        }
      }
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
