
#' Setup plan for the given hardware settings.
#' 
#' @param hardware hardware settings object
#' @return nothing
#' @export
setupPlan <- function(hardware) {
  # Reset handlers and plan
  future::plan(future::sequential)
  
  # Prepare multi-threading simulation    
  if (hardware@parallel) {
    future::plan(future::multisession, workers=hardware@cpu)
  }
}

