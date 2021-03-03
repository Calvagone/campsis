
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(expectOneForAll(object, c("amount", "compartment")))
}

#' 
#' Treatment entry class.
#' 
setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer"
  ),
  contains = "time_entry",
  prototype=prototype(compartment=as.integer(NA)),
  validity=checkTreatmentEntry
)

#_______________________________________________________________________________
#----                           bolus class                                 ----
#_______________________________________________________________________________

checkBolus <- function(object) {
  return(TRUE)
}

#' 
#' Bolus class.
#' 
#' @export
setClass(
  "bolus",
  representation(
  ),
  contains = "treatment_entry",
  validity=checkBolus
)

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

checkInfusion <- function(object) {
  return(expectOneForAll(object, c("duration")))
}

#' 
#' Infusion class.
#' 
#' @export
setClass(
  "infusion",
  representation(
    duration = "numeric"
  ),
  contains = "treatment_entry",
  validity=checkInfusion
)


#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________


setMethod("convert", signature = c("bolus"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.integer(0), CMT=object@compartment))
})

setMethod("convert", signature = c("infusion"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.integer(-2), CMT=object@compartment))
})

