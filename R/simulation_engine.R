#_______________________________________________________________________________
#----                           engine_type                                 ----
#_______________________________________________________________________________

#'
#' Simulation engine class.
#'
#' @export
setClass(
  "simulation_engine",
  representation(
  )
)


#'
#' rxode2 engine class.
#'
#' @export
setClass(
  "rxode_engine",
  representation(
  ),
  contains="simulation_engine"
)

#'
#' mrgsolve engine class.
#'
#' @export
setClass(
  "mrgsolve_engine",
  representation(
  ),
  contains="simulation_engine"
)
