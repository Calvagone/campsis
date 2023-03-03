
#' Setup plan for the given hardware settings.
#' 
#' @param hardware hardware settings object
#' @return nothing
#' @importFrom future multisession plan sequential
#' @export
setupPlan <- function(hardware) {
  # Reset handlers and plan
  future::plan(future::sequential)
  
  # Prepare multi-threading simulation    
  if (hardware@cpu > 1) {
    future::plan(future::multisession, workers=hardware@cpu)
  }
}

getFurrrScheduling <- function(parallel) {
  return(ifelse(parallel, 1, 0))
}