
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

#'
#' Create an observation.
#'
#' @param time observation time, numeric
#' @param compartment compartment index, integer
#' @return an observation
#' @export
Observation <- function(time, compartment=NA) {
  return(new("observation", time=time, compartment=as.integer(compartment)))
}

setMethod("getName", signature = c("observation"), definition = function(x) {
  return(paste0("OBS [", "TIME=", x@time, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________

setMethod("convert", signature = c("observation"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    obsCmt <- config@def_obs_cmt
  } else {
    obsCmt <- object@compartment
  }
  return(data.frame(TIME=object@time, EVID=as.integer(0), MDV=as.integer(0), DV=".", AMT=as.numeric(NA), RATE=as.integer(NA), CMT=obsCmt, DOSENO=as.integer(NA)))
})
