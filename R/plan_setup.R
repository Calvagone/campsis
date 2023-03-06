
#' Setup plan for the given hardware settings.
#' 
#' @param hardware hardware settings object
#' @return nothing
#' @importFrom future multisession plan sequential
#' @export
setupPlanDefault <- function(hardware) {
  # Reset plan
  future::plan(future::sequential)
  
  # Prepare multi-threading simulation    
  if (hardware@cpu > 1) {
    future::plan(future::multisession, workers=hardware@cpu)
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
