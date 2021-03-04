
#_______________________________________________________________________________
#----                     observation class                                ----
#_______________________________________________________________________________

checkObservation <- function(object) {
  return(expectOne(object, "compartment"))
}

#' 
#' Observation entry class.
#' 
#' @export
setClass(
  "observation",
  representation(
    compartment = "integer"
  ),
  contains = "time_entry",
  prototype=prototype(compartment=as.integer(NA)),
  validity=checkObservation
)

setMethod("getName", signature = c("observation"), definition = function(x) {
  return(paste0("OBS [", "TIME=", x@time, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________

setMethod("convert", signature = c("observation"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=as.integer(0), MDV=as.integer(0), DV=".", AMT=as.numeric(NA), RATE=as.integer(NA), CMT=object@compartment))
})