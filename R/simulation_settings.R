#_______________________________________________________________________________
#----                       simulation_settings class                       ----
#_______________________________________________________________________________

#' 
#' Simulation settings class.
#' 
#' @slot hardware hardware settings object
#' @slot solver solver settings object
#' @slot nocb NOCB settings object
#' @export
setClass(
  "simulation_settings",
  representation(
    hardware="hardware_settings",
    solver="solver_settings",
    nocb="nocb_settings"
  ),
  prototype=prototype(hardware=Hardware(), solver=Solver(), nocb=NOCB())
)

#'
#' Create advanced simulation settings.
#'
#' @param ... other settings like hardware settings, solver settings, NOCB settings, etc.
#' @param cpu number of CPU's to use, default is 1
#' @param parallel enable parallel computing, logical value
#'
#' @return advanced simulation settings
#' @importFrom purrr detect
#' @export
Settings <- function(..., cpu=1, parallel=FALSE) {
  args <- list(...)
  
  # Check if hardware settings are specified
  hardware <- args %>% purrr::detect(~(is(.x, "hardware_settings")))
  if (is.null(hardware)) {
    hardware <- Hardware(cpu=cpu, parallel=parallel)
  }
  
  # Check if solver settings are specified
  solver <- args %>% purrr::detect(~(is(.x, "solver_settings")))
  if (is.null(solver)) {
    solver <- Solver()
  }
  
  # Check if NOCB settings are specified
  nocb <- args %>% purrr::detect(~(is(.x, "nocb_settings")))
  if (is.null(nocb)) {
    nocb <- NOCB()
  }
  
  return(new("simulation_settings", hardware=hardware, solver=solver, nocb=nocb))
}
