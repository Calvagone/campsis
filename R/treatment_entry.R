
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(expectOneForAll(object, c("amount", "compartment", "dose_number", "f", "lag")))
}

setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer",
    f = "distribution",
    lag = "distribution",
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
#' @param time treatment time(s), numeric value or vector
#' @param amount amount to give as bolus, numeric
#' @param compartment compartment index, integer
#' @param f fraction of dose amount, distribution
#' @param lag dose lag time, distribution
#' @return an observation
#' @export
Bolus <- function(time, amount, compartment=NA, f=NULL, lag=NULL) {
  if (time %>% length() > 1) {
    return(time %>% purrr::map(
       .f=~new("bolus", time=.x, amount=amount, compartment=as.integer(compartment),
               f=toExplicitDistribution(f), lag=toExplicitDistribution(lag))))
  } else {
    return(new("bolus", time=time, amount=amount, compartment=as.integer(compartment),
               f=toExplicitDistribution(f), lag=toExplicitDistribution(lag)))
  }
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
  validity=validateInfusion
)

#'
#' Create an infusion.
#'
#' @param time treatment time, numeric
#' @param amount total amount to infuse, numeric
#' @param compartment compartment index, integer
#' @param f fraction of infusion amount, distribution
#' @param lag infusion lag time, distribution
#' @param duration infusion duration, distribution
#' @param rate infusion rate, distribution
#' @return an infusion.
#' @export
Infusion <- function(time, amount, compartment=NA, f=NULL, lag=NULL, duration=NULL, rate=NULL) {
  if (time %>% length() > 1) {
    return(time %>% purrr::map(
      .f=~new("infusion", time=.x, amount=amount, compartment=as.integer(compartment),
              f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
              duration=toExplicitDistribution(duration), rate=toExplicitDistribution(rate))))
  } else {
    return(new("infusion", time=time, amount=amount, compartment=as.integer(compartment),
               f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
               duration=toExplicitDistribution(duration), rate=toExplicitDistribution(rate)))
  }
}

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(paste0("INFUSION [", "TIME=", x@time, ", ", "AMOUNT=", x@amount, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

sampleTrtDistribution <- function(distribution, n, default) {
  if (is(distribution, "undefined_distribution")) {
    return(default) # Single value returned
  } else {
    return((distribution %>% sample(n))@sampled_values)
  }
}

setMethod("sample", signature = c("bolus", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- processExtraArg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- processExtraArg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  f <- sampleTrtDistribution(object@f, n, default=1)
  lag <- sampleTrtDistribution(object@lag, n, default=0)
  
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }

  return(tibble::tibble(ID=as.integer(ids), ARM=as.integer(armID), TIME=object@time+lag, EVID=as.integer(1), MDV=as.integer(1),
                    AMT=object@amount*f, CMT=depotCmt, RATE=as.numeric(0), DOSENO=object@dose_number, IS_INFUSION=FALSE, EVENT_RELATED=as.integer(FALSE)))
})

setMethod("sample", signature = c("infusion", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- processExtraArg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- processExtraArg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  f <- sampleTrtDistribution(object@f, n, default=1)
  lag <- sampleTrtDistribution(object@lag, n, default=0)
  
  
  if (is.na(object@compartment)) {
    depotCmt <- config@def_depot_cmt
  } else {
    depotCmt <- object@compartment
  }
  retValue <- tibble::tibble(ID=as.integer(ids), ARM=as.integer(armID), TIME=object@time+lag, EVID=as.integer(1), MDV=as.integer(1),
                         AMT=object@amount*f, CMT=depotCmt, RATE=as.numeric(NA), DOSENO=object@dose_number, IS_INFUSION=TRUE, EVENT_RELATED=as.integer(FALSE))
  
  # Duration or rate
  if (!is(object@duration, "undefined_distribution")) {
    duration <- sampleTrtDistribution(object@duration, n, default=0)
    retValue <- retValue %>% dplyr::mutate(RATE=AMT/duration)
  } else if (!is(object@rate, "undefined_distribution")) {
    rate <- sampleTrtDistribution(object@rate, n, default=0)
    retValue <- retValue %>% dplyr::mutate(RATE=rate)
  }
  
  return(retValue)
})

