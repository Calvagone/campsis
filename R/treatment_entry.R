
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(c(expectOneForAll(object, c("amount", "dose_number", "f", "lag")),
           expectZeroOrMore(object, "compartment")))
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
  return(expectOneOrMore(object, "time"))
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
#----                       bolus_wrapper class                             ----
#_______________________________________________________________________________

checkBolusWrapper <- function(object) {
  return(c(expectOneForAll(object, c("ii", "addl", "ref"))))
}

#' 
#' Bolus wrapper class.
#' 
#' @export
setClass(
  "bolus_wrapper",
  representation(
    ii = "numeric",
    addl = "integer",
    ref = "character"
  ),
  contains="bolus",
  validity=checkBolusWrapper
)

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
#' @param wrap if TRUE, the bolus wrapper will be stored as is in the dataset, otherwise,
#'  it will be split into a list of infusions distinct in time. Default is TRUE.
#' @param ref wrapper reference, used to identify the wrapper in the dataset, single character value
#' @return a single bolus or a list of boluses
#' @export
Bolus <- function(time, amount, compartment=NULL, f=NULL, lag=NULL, ii=NULL, addl=NULL, wrap=TRUE, ref=NULL) {
  iiAddl <- checkIIandADDL(time=time, ii=ii, addl=addl)
  ref <- ifelse(is.null(ref), as.character(NA), as.character(ref))
  wrapper <- new("bolus_wrapper", time=time, amount=amount, compartment=as.character(compartment),
                 f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
                 ii=iiAddl$ii, addl=iiAddl$addl, ref=ref)
  if (wrap) {
    return(wrapper)
  } else {
    return(unwrapTreatment(wrapper))  
  }
}

setMethod("getName", signature = c("bolus"), definition = function(x) {
  return(sprintf("BOLUS [TIME=%s, CMT=%s]", as.character(x@time), getTreatmentEntryCmtString(x)))
})

setMethod("getName", signature = c("bolus_wrapper"), definition = function(x) {
  return(sprintf("BOLUS WRAPPER [REF=%s]", as.character(x@ref)))
})

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

validateInfusion <- function(object) {
  return(c(expectOneOrMore(object, "time"), expectOneForAll(object, c("duration", "rate"))))
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

#_______________________________________________________________________________
#----                       infusion_wrapper class                          ----
#_______________________________________________________________________________

checkInfusionWrapper <- function(object) {
  return(c(expectOneForAll(object, c("ii", "addl", "ref"))))
}

#' 
#' Infusion wrapper class.
#' 
#' @export
setClass(
  "infusion_wrapper",
  representation(
    ii = "numeric",
    addl = "integer",
    ref = "character"
  ),
  contains="infusion",
  validity=checkInfusionWrapper
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
#' @param wrap if TRUE, the infusion wrapper will be stored as is in the dataset, otherwise,
#'  it will be split into a list of infusions distinct in time. Default is TRUE.
#' @param ref wrapper reference, used to identify the wrapper in the dataset, single character value
#' @return a single infusion or a list of infusions.
#' @export
Infusion <- function(time, amount, compartment=NULL, f=NULL, lag=NULL, duration=NULL, rate=NULL, ii=NULL, addl=NULL, wrap=TRUE, ref=NULL) {
  iiAddl <- checkIIandADDL(time=time, ii=ii, addl=addl)
  ref <- ifelse(is.null(ref), as.character(NA), as.character(ref))
  wrapper <- new("infusion_wrapper", time=time, amount=amount, compartment=as.character(compartment),
                 f=toExplicitDistribution(f), lag=toExplicitDistribution(lag),
                 duration=toExplicitDistribution(duration), rate=toExplicitDistribution(rate),
                 ii=iiAddl$ii, addl=iiAddl$addl, ref=ref)
  if (wrap) {
    return(wrapper)
  } else {
    return(unwrapTreatment(wrapper))  
  }
}

setMethod("getName", signature = c("infusion"), definition = function(x) {
  return(sprintf("INFUSION [TIME=%s, CMT=%s]", as.character(x@time), getTreatmentEntryCmtString(x)))
})

setMethod("getName", signature = c("infusion_wrapper"), definition = function(x) {
  return(sprintf("INFUSION WRAPPER [REF=%s]", as.character(x@ref)))
})

#_______________________________________________________________________________
#----                             utilities                                 ----
#_______________________________________________________________________________

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
    return(list(ii=as.numeric(NA), addl=as.integer(NA)))
  } else {
    assertthat::assert_that(!is.null(ii), msg="ii can't be NULL if addl is specified")
    assertthat::assert_that(!is.null(addl), msg="addl can't be NULL if ii is specified")
    
    assertthat::assert_that(is.numeric(ii) && length(ii)==1 && !is.na(ii), msg="ii must be a single numeric value")
    assertthat::assert_that(ii > 0 , msg="ii must be higher than 0")
    
    assertthat::assert_that(is.numeric(addl) && length(addl)==1 && addl%%1==0 && !is.na(addl), msg="addl must be a single integer value")
    assertthat::assert_that(addl >= 0 , msg="addl must be positive")
    
    assertthat::assert_that(length(time)==1, msg="time must be a single numeric value if used with ii and addl")
    return(list(ii=as.numeric(ii), addl=as.integer(addl)))
  }
}

getTreatmentEntryCmtString <- function(object, vector=FALSE) {
  if (object@compartment %>% length() == 0) {
    str <- "DEFAULT"
  } else {
    str <- sprintf("%s", paste0(object@compartment, collapse=","))
    if (vector) str <- sprintf("c(%s)", str)
  }
  return(str)
}

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
  
  if (length(object@compartment)==0) {
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
  
  
  if (length(object@compartment)==0) {
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

#_______________________________________________________________________________
#----                          unwrapTreatment                              ----
#_______________________________________________________________________________

unwrapTreatmentDelegate <- function(object, type) {
  time <- object@time
  amount <- object@amount
  compartment <- object@compartment
  f <- object@f
  lag <- object@lag
  ii <- object@ii
  addl <- object@addl

  if (time %>% length() > 1) {
    retValue <- time %>% 
      purrr::map(~new(type, time=.x, amount=amount, compartment=compartment, f=f, lag=lag))
  } else {
    if (is.na(addl)) {
      retValue <- new(type, time=time, amount=amount, compartment=compartment, f=f, lag=lag)
    } else {
      retValue <- (seq_len(addl + 1) - 1) %>%
        purrr::map(~new(type, time=time + ii*.x, amount=amount, compartment=compartment, f=f, lag=lag))
    }
  }
  if (type=="infusion") {
    if (isS4(retValue)) {
      # Infusion object
      retValue@duration <- object@duration
      retValue@rate <- object@rate
    } else {
      # List of infusion objects
      retValue <- retValue %>%
        purrr::map(.f=function(x) {
          x@duration <- object@duration
          x@rate <- object@rate
          return(x)
        })
    }
  }
  return(retValue)
}

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature = c("bolus"), definition = function(object) {
  return(object)
})

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature = c("infusion"), definition = function(object) {
  return(object)
})

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature = c("bolus_wrapper"), definition = function(object) {
  return(unwrapTreatmentDelegate(object, type="bolus"))
})

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature = c("infusion_wrapper"), definition = function(object) {
  return(unwrapTreatmentDelegate(object, type="infusion"))
})
