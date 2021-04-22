
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(expectOneForAll(object, c("amount", "compartment", "dose_number", "fraction", "lag")))
}

setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer",
    fraction = "distribution",
    lag = "distribution",
    dose_number = "integer" # Transient
  ),
  contains = "time_entry",
  prototype=prototype(compartment=as.integer(NA), dose_number=as.integer(NA), fraction=NULL, lag=NULL),
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
#' @param fraction fraction of dose, distribution
#' @param lag dose lag time, distribution
#' @return an observation
#' @export
Bolus <- function(time, amount, compartment=NA, fraction=NULL, lag=NULL) {
  return(new("bolus", time=time, amount=amount, compartment=as.integer(compartment), fraction=fraction, lag=lag))
}

setMethod("getName", signature = c("bolus"), definition = function(x) {
  return(paste0("BOLUS [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

validateInfusion <- function(object) {
  return(expectOneForAll(object, c("duration", "rate")))
}

#' @export
setClass(
  "infusion",
  representation(
    duration = "distribution",
    rate = "distribution"
  ),
  contains = "treatment_entry",
  prototype=prototype(duration=NULL, lagtime=NULL),
  validity=validateInfusion
)

#'
#' Create an infusion.
#'
#' @param time treatment time, numeric
#' @param amount total amount to infuse, numeric
#' @param compartment compartment index, integer
#' @param fraction fraction of infusion amount, distribution
#' @param lag infusion lag time, distribution
#' @param duration infusion duration, distribution
#' @param rate infusion rate, distribution
#' @return an infusion.
#' @export
Infusion <- function(time, amount, compartment=NA, fraction=NULL, lag=NULL, duration=NULL, rate=NULL) {
  return(new("infusion", time=time, amount=amount, compartment=as.integer(compartment),
             fraction=fraction, lag=lag, duration=duration, rate=rate))
}

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(paste0("INFUSION [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________


setMethod("sample", signature = c("bolus", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE)
  maxID <- processExtraArg(args, name="maxID", mandatory=TRUE)
  ids <- seq_len(subjects) + maxID - subjects
  
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }

  return(data.frame(ID=ids, TIME=object@time, EVID=as.integer(1), MDV=as.integer(1),
                    AMT=object@amount, CMT=depotCmt, DOSENO=object@dose_number, IS_INFUSION=FALSE))
})

setMethod("sample", signature = c("infusion", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE)
  maxID <- processExtraArg(args, name="maxID", mandatory=TRUE)
  ids <- seq_len(subjects) + maxID - subjects
  
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }
  return(data.frame(ID=ids, TIME=object@time, EVID=as.integer(1), MDV=as.integer(1),
                    AMT=object@amount, CMT=depotCmt, DOSENO=object@dose_number, IS_INFUSION=TRUE))
})

