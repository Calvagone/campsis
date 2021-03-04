
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

setMethod("getName", signature = c("bolus"), definition = function(x) {
  return(paste0("BOLUS [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

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

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(paste0("INFUSION [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________


setMethod("convert", signature = c("bolus", "config"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    depotCmt <- config@default_depot_cmt
  } else {
    depotCmt <- object@compartment
  }
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.integer(0), CMT=depotCmt))
})

setMethod("convert", signature = c("infusion", "config"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    depotCmt <- config@default_depot_cmt
  } else {
    depotCmt <- object@compartment
  }
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.integer(-2), CMT=depotCmt))
})

