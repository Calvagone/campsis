
#_______________________________________________________________________________
#----                     dataset_entry class                               ----
#_______________________________________________________________________________

checkDatasetEntry <- function(object) {
  return(checkReturn(expectOneForAll(object, "time")))
}

setClass(
  "dataset_entry",
  representation(
    time = "numeric"
  ),
  validity=checkDatasetEntry
)

#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkArms <- function(object) {
  errors <- character()
  if (length(object@arms) != 0) {
    for(arm in object@arms) {
      errors <- addError(checkArm(arm), errors)
    }
  }
  return(errors)
}

checkTreatmentEntry <- function(object) {
  check1 <- expectOneForAll(object, c("amount", "compartment"))
  check2 <- checkArms(object)
  return(checkReturn(c(check1, check2)))
}

#' 
#' Treatment entry class.
#' 
setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer",
    arms = "list"
  ),
  contains = "dataset_entry",
  prototype=prototype(compartment=as.integer(NA), arms=list()),
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
  check1 <- expectOneForAll(object, c("duration"))
  check2 <- checkArms(object)
  return(checkReturn(c(check1, check2)))
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
