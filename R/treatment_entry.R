
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(expectOneForAll(object, c("amount", "compartment", "dose_number")))
}

setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer",
    dose_number = "integer" # Transient
  ),
  contains = "time_entry",
  prototype=prototype(compartment=as.integer(NA), dose_number=as.integer(NA)),
  validity=checkTreatmentEntry
)

#_______________________________________________________________________________
#----                           bolus class                                 ----
#_______________________________________________________________________________

checkBolus <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "bolus",
  representation(
  ),
  contains = "treatment_entry",
  validity=checkBolus
)

#'
#' Create a bolus.
#'
#' @param time treatment time, numeric
#' @param amount amount to give as bolus, numeric
#' @param compartment compartment index, integer
#' @return an observation
#' @export
Bolus <- function(time, amount, compartment=NA) {
  return(new("bolus", time=time, amount=amount, compartment=as.integer(compartment)))
}

setMethod("getName", signature = c("bolus"), definition = function(x) {
  return(paste0("BOLUS [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

validateInfusion <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "infusion",
  representation(
  ),
  contains = "treatment_entry",
  validity=validateInfusion
)

#'
#' Create an infusion.
#'
#' @param time treatment time, numeric
#' @param amount total amount to infuse, numeric
#' @param compartment compartment index, integer
#' @return an infusion.
#' @export
Infusion <- function(time, amount, compartment=NA) {
  return(new("infusion", time=time, amount=amount, compartment=as.integer(compartment)))
}

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(paste0("INFUSION [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________


setMethod("convert", signature = c("bolus", "dataset_config"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }

  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1),
                    AMT=object@amount, CMT=depotCmt, DOSENO=object@dose_number, IS_INFUSION=FALSE))
})

setMethod("convert", signature = c("infusion", "dataset_config"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1),
                    AMT=object@amount, CMT=depotCmt, DOSENO=object@dose_number, IS_INFUSION=TRUE))
})

