
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

checkInfusion <- function(object) {
  check1 <- expectOneForAll(object, c("duration", "rate"))
  check2 <- if(xor(is.na(object@duration), is.na(object@rate))) {character()} else {"Please specify either the duration or rate."}
  return(c(check1, check2))
}

#' @export
setClass(
  "infusion",
  representation(
    duration = "numeric",
    rate = "numeric"
  ),
  contains = "treatment_entry",
  prototype=prototype(duration=as.numeric(NA), rate=as.numeric(NA)),
  validity=checkInfusion
)

#'
#' Create an infusion bolus.
#'
#' @param time treatment time, numeric
#' @param amount amount to give as bolus, numeric
#' @param duration infusion duration
#' @param rate infusion rate
#' @param compartment compartment index, integer
#' @return an observation
#' @export
Infusion <- function(time, amount, duration=NA, rate=NA, compartment=NA) {
  return(new("infusion", time=time, amount=amount, duration=as.numeric(duration), rate=as.numeric(rate), compartment=as.integer(compartment)))
}

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(paste0("INFUSION [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                            getRate                                    ----
#_______________________________________________________________________________

#' Get infusion rate.
#' 
#' @param object infusion
#' @return the rate of this infusion
#' @export
getRate <- function(object) {
  stop("No default function is provided")
}

setGeneric("getRate", function(object) {
  standardGeneric("getRate")
})

setMethod("getRate", signature = c("infusion"), definition = function(object) {
  if (is.na(object@duration)) {
    return(object@rate)
  } else {
    return(object@amount/object@duration)
  }
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
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.numeric(0), CMT=depotCmt, DOSENO=object@dose_number))
})

setMethod("convert", signature = c("infusion", "dataset_config"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }
  rate <- object %>% getRate()
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=rate, CMT=depotCmt, DOSENO=object@dose_number))
})

