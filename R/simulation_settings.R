#_______________________________________________________________________________
#----                       simulation_settings class                       ----
#_______________________________________________________________________________

#' 
#' Simulation settings class.
#' 
#' @slot hardware hardware settings object
#' @slot solver solver settings object
#' @slot nocb NOCB settings object
#' @slot declare declare settings (mrgsolve only)
#' @slot internal internal settings
#' @export
setClass(
  "simulation_settings",
  representation(
    hardware="hardware_settings",
    solver="solver_settings",
    nocb="nocb_settings",
    declare="declare_settings",
    internal="internal_settings"
  ),
  prototype=prototype(hardware=Hardware(), solver=Solver(), nocb=NOCB(), declare=Declare())
)

#'
#' Create advanced simulation settings.
#'
#' @param ... any required settings: hardware settings, solver settings, NOCB settings,
#'  declare settings, etc.
#' @return advanced simulation settings
#' @importFrom purrr detect
#' @export
Settings <- function(...) {
  args <- list(...)
  
  # Check if hardware settings are specified
  hardware <- args %>% purrr::detect(~(is(.x, "hardware_settings")))
  if (is.null(hardware)) {
    hardware <- Hardware()
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
  
  # Check if declare settings are specified
  declare <- args %>% purrr::detect(~(is(.x, "declare_settings")))
  if (is.null(declare)) {
    declare <- Declare()
  }
  
  return(new("simulation_settings", hardware=hardware, solver=solver, nocb=nocb, declare=declare))
}
