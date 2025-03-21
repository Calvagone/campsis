
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(c(expectOneForAll(object, c("amount", "dose_number", "f", "lag")),
           expectOneOrMore(object, "compartment")))
}

setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "character",
    f = "distribution",
    lag = "distribution",
    dose_number = "integer" # Transient
  ),
  contains = "time_entry",
  prototype=prototype(compartment=as.character(NA), dose_number=as.integer(NA)),
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

#'
#' Check ii and addl arguments in addition to time.
#'
#' @param time treatment time(s)
#' @param ii interdose interval
#' @param addl number of additional doses
#' @return no return value
#' @importFrom assertthat assert_that
#' @keywords internal
#'
checkIIandADDL <- function(time, ii, addl) {
  if (is.null(ii) && is.null(addl)) {
    # Don't need to check anything
  } else {
    assertthat::assert_that(!is.null(ii), msg="ii can't be NULL if addl is specified")
    assertthat::assert_that(!is.null(addl), msg="addl can't be NULL if ii is specified")
    
    assertthat::assert_that(is.numeric(ii) && length(ii)==1 && !is.na(ii), msg="ii must be a single numeric value")
    assertthat::assert_that(ii > 0 , msg="ii must be higher than 0")
    
    assertthat::assert_that(is.numeric(addl) && length(addl)==1 && addl%%1==0 && !is.na(addl), msg="addl must be a single integer value")
    assertthat::assert_that(addl >= 0 , msg="addl must be positive")
    
    assertthat::assert_that(length(time)==1, msg="time must be a single numeric value if used with ii and addl")
  }
}

#'
#' Create one or several bolus(es).
#'
#' @param time treatment time(s), numeric value or vector. First treatment time if used together with ii and addl.
#' @param amount amount to give as bolus, single numeric value
#' @param compartment compartment index or name to give the bolus(es). A vector of integers or names can be used for a complex model administration.
#' @param f fraction of dose amount, distribution
#' @param lag dose lag time, distribution
#' @param ii interdose interval, requires argument 'time' to be a single numeric value
#' @param addl number of additional doses, requires argument 'time' to be a single integer value
#' @return a single bolus or a list of boluses
#' @importFrom purrr map
#' @export
Bolus <- function(time, amount, compartment=NA, f=NULL, lag=NULL, ii=NULL, addl=NULL) {
  checkIIandADDL(time=time, ii=ii, addl=addl)
  if (time %>% length() > 1) {
    return(time %>% purrr::map(
       .f=~new("bolus", time=.x, amount=amount, compartment=as.character(compartment),
               f=toExplicitDistribution(f), lag=toExplicitDistribution(lag))))
  } else {
    if (is.null(addl)) {
      return(new("bolus", time=time, amount=amount, compartment=as.character(compartment),
                 f=toExplicitDistribution(f), lag=toExplicitDistribution(lag)))
    } else {
      return((seq_len(addl + 1) - 1) %>% purrr::map(
        .f=~new("bolus", time=time + ii*.x, amount=amount, compartment=as.character(compartment),
                f=toExplicitDistribution(f), lag=toExplicitDistribution(lag))))
    }
  }
}

setMethod("getName", signature = c("bolus"), definition = function(x) {
  return(paste0("BOLUS [", "TIME=", x@time, ", ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

validateInfusion <- function(object) {
  return(expectOneForAll(object, c("duration", "rate")))
}

#' 
#' Infusion class.
#' 
#' @slot duration infusion duration, distribution
#' @slot rate infusion rate, distribution
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
#' Create one or several infusion(s).
#'
#' @param time treatment time(s), numeric value or vector. First treatment time if used together with ii and addl.
#' @param amount total amount to infuse, numeric
#' @param compartment compartment index or name to give the infusion(s). A vector of integers or names can be used for a complex model administration.
#' @param f fraction of infusion amount, distribution
#' @param lag infusion lag time, distribution
#' @param duration infusion duration, distribution
#' @param rate infusion rate, distribution
#' @param ii interdose interval, requires argument 'time' to be a single numeric value
#' @param addl number of additional doses, requires argument 'time' to be a single integer value
#' @return a single infusion or a list of infusions.
#' @importFrom purrr map
#' @export
Infusion <- function(time, amount, compartment=NA, f=NULL, lag=NULL, duration=NULL, rate=NULL, ii=NULL, addl=NULL) {
  checkIIandADDL(time=time, ii=ii, addl=addl)
  if (time %>% length() > 1) {
    return(time %>% purrr::map(
      .f=~new("infusion", time=.x, amount=amount, compartment=as.character(compartment),
              f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
              duration=toExplicitDistribution(duration), rate=toExplicitDistribution(rate))))
  } else {
    if (is.null(addl)) {
      return(new("infusion", time=time, amount=amount, compartment=as.character(compartment),
                 f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
                 duration=toExplicitDistribution(duration), rate=toExplicitDistribution(rate)))
    } else {
      return((seq_len(addl + 1) - 1) %>% purrr::map(
        .f=~new("infusion", time=time + ii*.x, amount=amount, compartment=as.character(compartment),
                f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
                duration=toExplicitDistribution(duration), rate=toExplicitDistribution(rate))))
    }
  }
}

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(paste0("INFUSION [", "TIME=", x@time, ", ", "CMT=", x@compartment, "]"))
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

#' @rdname sample
setMethod("sample", signature = c("bolus", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- processExtraArg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- processExtraArg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  needsDV <- processExtraArg(args, name="needsDV", mandatory=TRUE, default=FALSE)
  f <- sampleTrtDistribution(object@f, n, default=1)
  lag <- sampleTrtDistribution(object@lag, n, default=0)
  
  if (is.na(object@compartment)) {
    depotCmt <- as.character(config@def_depot_cmt)
  } else {
    depotCmt <- object@compartment
  }
  
  retValue <- tibble::tibble(
    ID=rep(as.integer(ids), each=length(depotCmt)), ARM=as.integer(armID), TIME=object@time+lag, 
    EVID=as.integer(1), MDV=as.integer(1), AMT=object@amount*f, CMT=depotCmt, RATE=as.numeric(0), DOSENO=object@dose_number,
    INFUSION_TYPE=as.integer(0), EVENT_RELATED=as.integer(FALSE)
  )
  if (needsDV) {
    retValue <- retValue %>% tibble::add_column(DV=as.numeric(0), .before="INFUSION_TYPE")
  }
  return(retValue)
})

#' @rdname sample
setMethod("sample", signature = c("infusion", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- processExtraArg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- processExtraArg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  needsDV <- processExtraArg(args, name="needsDV", mandatory=TRUE, default=FALSE)
  f <- sampleTrtDistribution(object@f, n, default=1)
  lag <- sampleTrtDistribution(object@lag, n, default=0)
  
  
  if (is.na(object@compartment)) {
    depotCmt <- as.character(config@def_depot_cmt)
  } else {
    depotCmt <- object@compartment
  }
  retValue <- tibble::tibble(
    ID=rep(as.integer(ids), each=length(depotCmt)), ARM=as.integer(armID), TIME=object@time+lag, 
    EVID=as.integer(1), MDV=as.integer(1), AMT=object@amount*f, CMT=depotCmt, RATE=as.numeric(NA), DOSENO=object@dose_number,
    INFUSION_TYPE=as.integer(-2), EVENT_RELATED=as.integer(FALSE)
  )
  
  # Duration or rate
  if (!is(object@duration, "undefined_distribution")) {
    duration <- sampleTrtDistribution(object@duration, n, default=0)
    retValue <- retValue %>% dplyr::mutate(RATE=.data$AMT/duration, INFUSION_TYPE=as.integer(-2))
  } else if (!is(object@rate, "undefined_distribution")) {
    rate <- sampleTrtDistribution(object@rate, n, default=0)
    retValue <- retValue %>% dplyr::mutate(RATE=rate, INFUSION_TYPE=as.integer(-1))
  }
  if (needsDV) {
    retValue <- retValue %>% tibble::add_column(DV=as.numeric(0), .before="INFUSION_TYPE")
  }
  return(retValue)
})

