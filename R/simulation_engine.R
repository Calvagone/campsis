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
#' RxODE/rxode2 engine class.
#' 
#' @slot rxode2 logical field to indicate if CAMPSIS should use rxode2 (field set to TRUE)
#'  or RxODE (field set to FALSE). Default is TRUE.
#' @export
setClass(
  "rxode_engine",
  representation(
    rxode2="logical"
  ),
  contains="simulation_engine",
  prototype=prototype(rxode2=TRUE)
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
