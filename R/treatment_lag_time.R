
#_______________________________________________________________________________
#----                     treatment_lag_time class                          ----
#_______________________________________________________________________________

validateLagTime <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "treatment_lag_time",
  representation(
  ),
  contains = "treatment_characteristic",
  validity=validateLagTime
)

#'
#' Create a lag time for the specified compartment.
#'
#' @param compartment compartment number
#' @param distribution distribution
#' @return lag time
#' @export
TreatmentLagTime <- function(compartment, distribution) {
  return(new("treatment_lag_time", compartment=as.integer(compartment), distribution=distribution))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________


setMethod("getName", signature = c("treatment_lag_time"), definition = function(x) {
  return(paste0("LAG_TIME [", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                         getColumnName                                 ----
#_______________________________________________________________________________

setMethod("getColumnName", signature = c("treatment_lag_time"), definition = function(x) {
  return(paste0("ALAG", x@compartment))
})