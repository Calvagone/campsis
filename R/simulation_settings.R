#_______________________________________________________________________________
#----                       simulation_settings class                       ----
#_______________________________________________________________________________

#' 
#' Simulation settings class.
#' 
#' @slot hardware_settings hardware settings object
#' @slot nocb_settings NOCB settings object
#' @export
setClass(
  "simulation_settings",
  representation(
    hardware="hardware_settings",
    nocb="nocb_settings"
  ),
  prototype=prototype(hardware=Hardware(), nocb=NOCB())
)

#'
#' Create advanced simulation settings.
#'
#' @param ... other settings like hardware settings, NOCB settings, etc.
#' @param cpu number of CPU's to use, default is 6
#' @param parallel enable parallel computing, logical value
#'
#' @return advanced simulation settings
#' @importFrom purrr detect
#' @export
Settings <- function(..., cpu=6, parallel=FALSE) {
  args <- list(...)
  nocb <- args %>% purrr::detect(~(is(.x, "nocb_settings")))
  if (is.null(nocb)) {
    nocb <- NOCB()
  }
  hardware <- args %>% purrr::detect(~(is(.x, "hardware_settings")))
  if (is.null(hardware)) {
    hardware <- Hardware(cpu=cpu, parallel=parallel)
  }
  return(new("simulation_settings", hardware=hardware, nocb=nocb))
}
