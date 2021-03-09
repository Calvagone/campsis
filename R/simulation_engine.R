#_______________________________________________________________________________
#----                           engine_type                                 ----
#_______________________________________________________________________________

#' @export
setClass(
  "simulation_engine",
  representation(
  )
)


#' @export
setClass(
  "rxode_engine",
  representation(
  ),
  contains="simulation_engine" 
)