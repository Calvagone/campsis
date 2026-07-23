
#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkTreatmentEntry <- function(object) {
  return(c(expectOneForAll(object, c("amount", "dose_number", "ref")),
           expectOneOrMore(object, c("f", "lag")),
           expectZeroOrMore(object, "compartment")))
}

setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "character",
    f = "list",               # Distribution list
    lag = "list",             # Distribution list
    dose_number = "integer",  # Transient
    ref = "character"         # Reference name
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
  return(c(expectOneForAll(object, c("ii", "addl"))))
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
    rep = "repeated_schedule"
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
#' @param f fraction of dose amount, list of distributions (one per compartment)
#' @param lag dose lag time, list of distributions (one per compartment)
#' @param ii inter-dose interval, requires argument 'time' to be a single numeric value
#' @param addl number of additional doses, requires argument 'time' to be a single integer value
#' @param wrap if TRUE, the bolus wrapper will be stored as is in the dataset, otherwise,
#'  it will be split into a list of boluses distinct in time. Default is TRUE.
#' @param ref any reference name used to identify this bolus, single character value
#' @param rep repeat the base dosing schedule several times, a 'repeated schedule' object is expected. Default is NULL (no repetition).
#' @return a single bolus or a list of boluses
#' @export
Bolus <- function(time, amount, compartment=NULL, f=NULL, lag=NULL, ii=NULL, addl=NULL, wrap=TRUE, ref=NULL, rep=NULL) {
  iiAddl <- checkIIandADDL(time=time, ii=ii, addl=addl)
  cmtNo <- ifelse(length(compartment)==0, 1, length(compartment))

  wrapper <- new("bolus_wrapper", time=time, amount=amount, compartment=as.character(compartment),
                 f=toExplicitDistributionList(f, cmtNo=cmtNo), lag=toExplicitDistributionList(lag, cmtNo=cmtNo),
                 ii=iiAddl$ii, addl=iiAddl$addl, ref=processRefArg(ref), rep=processRepeatArg(rep, iiAddl))
  if (wrap) {
    return(wrapper)
  } else {
    return(unwrap_treatmentBase(object=wrapper, type="bolus", wrap=wrap)) 
  }
}

setMethod("get_name", signature = c("bolus"), definition = function(x) {
  return(sprintf("BOLUS [TIME=%s, CMT=%s]", as.character(x@time), getTreatmentEntryCmtString(x)))
})

setMethod("get_name", signature = c("bolus_wrapper"), definition = function(x) {
  return(sprintf("BOLUS WRAPPER [REF=%s]", as.character(x@ref)))
})

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

validateInfusion <- function(object) {
  return(c(expectOneOrMore(object, "time"),
           expectOneOrMore(object, c("duration", "rate"))))
}

#' 
#' Infusion class.
#' 
#' @slot duration infusion duration, distribution list
#' @slot rate infusion rate, distribution list
#' @export
setClass(
  "infusion",
  representation(
    duration = "list", # Distribution list
    rate = "list" # Distribution list
  ),
  contains = "treatment_entry",
  validity=validateInfusion
)

#_______________________________________________________________________________
#----                       infusion_wrapper class                          ----
#_______________________________________________________________________________

checkInfusionWrapper <- function(object) {
  return(c(expectOneForAll(object, c("ii", "addl"))))
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
    rep = "repeated_schedule"
  ),
  contains="infusion",
  validity=checkInfusionWrapper
)

#'
#' Create one or several infusion(s).
#'
#' @param time treatment time(s), numeric value or vector. First treatment time if used together with ii and addl.
#' @param amount amount to infuse, single numeric value
#' @param compartment compartment index or name to give the infusion(s). A vector of integers or names can be used for a complex model administration.
#' @param f fraction of infusion amount, list of distributions (one per compartment)
#' @param lag infusion lag time, , list of distributions (one per compartment)
#' @param duration infusion duration, list of distributions (one per compartment)
#' @param rate infusion rate, list of distributions (one per compartment)
#' @param ii inter-dose interval, requires argument 'time' to be a single numeric value
#' @param addl number of additional doses, requires argument 'time' to be a single integer value
#' @param wrap if TRUE, the infusion wrapper will be stored as is in the dataset, otherwise,
#'  it will be split into a list of infusions distinct in time. Default is TRUE.
#' @param ref any reference name used to identify this infusion, single character value
#' @param rep repeat the base dosing schedule several times, a 'repeated schedule' object is expected. Default is NULL (no repetition).
#' @return a single infusion or a list of infusions.
#' @export
Infusion <- function(time, amount, compartment=NULL, f=NULL, lag=NULL, duration=NULL, rate=NULL, ii=NULL, addl=NULL, wrap=TRUE, ref=NULL, rep=NULL) {
  iiAddl <- checkIIandADDL(time=time, ii=ii, addl=addl)
  cmtNo <- ifelse(length(compartment)==0, 1, length(compartment))

  wrapper <- new("infusion_wrapper", time=time, amount=amount, compartment=as.character(compartment),
                 f=toExplicitDistributionList(f, cmtNo=cmtNo), lag=toExplicitDistributionList(lag, cmtNo=cmtNo),
                 duration=toExplicitDistributionList(duration, cmtNo=cmtNo), rate=toExplicitDistributionList(rate, cmtNo=cmtNo),
                 ii=iiAddl$ii, addl=iiAddl$addl, ref=processRefArg(ref), rep=processRepeatArg(rep, iiAddl))
  if (wrap) {
    return(wrapper)
  } else {
    return(unwrap_treatmentBase(object=wrapper, type="infusion", wrap=wrap))  
  }
}

setMethod("get_name", signature = c("infusion"), definition = function(x) {
  return(sprintf("INFUSION [TIME=%s, CMT=%s]", as.character(x@time), getTreatmentEntryCmtString(x)))
})

setMethod("get_name", signature = c("infusion_wrapper"), definition = function(x) {
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

processRepeatArg <- function(rep, iiAddl) {
  if (is.null(rep)) rep <- new("undefined_schedule")
  if (is.numeric(rep) && !is.na(iiAddl$ii)) {
    rep <- CyclicSchedule(duration=iiAddl$ii*(iiAddl$addl + 1), repetitions=rep)
  }
  return(rep)
}

processRefArg <- function(ref) {
  ref <- ifelse(is.null(ref), as.character(NA), as.character(ref))
  return(ref)
}

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

bolusInfFromJSON <- function(object, json) {
  # Time unit pre-processing
  if (!is.null(json@data$time) && !is.null(json@data$time_unit)) {
    json@data$time <- convert_time(json@data$time, from=json@data$time_unit, to="hour")
    json@data$time_unit <- NULL
  }
  if (!is.null(json@data$ii) && !is.null(json@data$ii_unit)) {
    json@data$ii <- convert_time(json@data$ii, from=json@data$ii_unit, to="hour")
    json@data$ii_unit <- NULL
  }
  if (!is.null(json@data$rep) && !is.null(json@data$rep$duration_unit)) {
    json@data$rep$duration <- convert_time(json@data$rep$duration, from=json@data$rep$duration_unit, to="hour")
    json@data$rep$duration_unit <- NULL
  }
  object@rep = new("undefined_schedule") # Default, no cycles
  object <- campsismod::map_json_properties_to_s4_slots(object, json)
  object@f <- toExplicitDistributionList(NULL, cmtNo=length(object@compartment))
  object@lag <- toExplicitDistributionList(NULL, cmtNo=length(object@compartment))
  object@ref <- processRefArg(NULL)
  return(object)
}

setMethod("load_from_json", signature=c("bolus_wrapper", "json_element"), definition=function(object, json) {
  return(bolusInfFromJSON(object=object, json=json))
})

setMethod("load_from_json", signature=c("infusion_wrapper", "json_element"), definition=function(object, json) {
  # Duration unit pre-processing
  if (!is.null(json@data$duration) && !is.null(json@data$duration_unit)) {
    json@data$duration <- convert_time(json@data$duration, from=json@data$duration_unit, to="hour")
    json@data$duration_unit <- NULL
  }
  # Process duration manually
  duration <- NULL
  if (!is.null(json@data$duration)) {
    duration <- json@data$duration
    json@data$duration <- NULL
  }
  object <- bolusInfFromJSON(object=object, json=json)
  object@duration <- toExplicitDistributionList(duration, cmtNo=length(object@compartment))
  object@rate <- toExplicitDistributionList(NULL, cmtNo=length(object@compartment))
  return(object)
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

sampleTrtDistribution <- function(distribution, n, default) {
  if (is(distribution, "undefined_distribution")) {
    return(rep(default, n)) # Single value returned
  } else {
    if (length(distribution@sampled_values)==0) {
      return((distribution %>% sample(n))@sampled_values)
    } else{
      return(distribution@sampled_values)
    }
  }
}

sampleTrtDistributions <- function(distributions, n, default, compartmentNo) {
  if (length(distributions)==1) {
    tmp <- seq_len(compartmentNo) %>%
      purrr::map(~sampleTrtDistribution(distribution=distributions[[1]], n=n, default=default))
  } else if (length(distributions)==compartmentNo) {
    tmp <- distributions %>%
      purrr::map(~sampleTrtDistribution(distribution=.x, n=n, default=default))
  } else {
    stop("Number of distributions must be 1 or equal to the number of compartments")
  }
  # This will interlace the list of vectors and return a unique numeric vector
  return(c(do.call(rbind, tmp)))
}

#' @rdname sample
setMethod("sample", signature = c("bolus", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- process_extra_arg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- process_extra_arg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- process_extra_arg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  needsDV <- process_extra_arg(args, name="needsDV", mandatory=TRUE, default=FALSE)
  
  if (length(object@compartment)==0) {
    depotCmt <- as.character(config@def_depot_cmt)
  } else {
    depotCmt <- object@compartment
  }
  compartmentNo <- length(depotCmt)
  
  f <- sampleTrtDistributions(distributions=object@f, n=n, default=1, compartmentNo=compartmentNo)
  lag <- sampleTrtDistributions(distributions=object@lag, n=n, default=0, compartmentNo=compartmentNo)

  retValue <- tibble::tibble(
    ID=rep(as.integer(ids), each=length(depotCmt)), ARM=as.integer(armID), TIME=object@time+lag, 
    EVID=as.integer(1), MDV=as.integer(1), AMT=object@amount*f, CMT=rep(depotCmt, length(ids)), RATE=as.numeric(0),
    DOSENO=object@dose_number, INFUSION_TYPE=as.integer(0), EVENT_RELATED=as.integer(FALSE)
  )
  if (needsDV) {
    retValue <- retValue %>% tibble::add_column(DV=as.numeric(0), .before="INFUSION_TYPE")
  }
  return(retValue)
})

#' @rdname sample
setMethod("sample", signature = c("infusion", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- process_extra_arg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- process_extra_arg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- process_extra_arg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  needsDV <- process_extra_arg(args, name="needsDV", mandatory=TRUE, default=FALSE)

  if (length(object@compartment)==0) {
    depotCmt <- as.character(config@def_depot_cmt)
  } else {
    depotCmt <- object@compartment
  }
  
  compartmentNo <- length(depotCmt)
  
  f <- sampleTrtDistributions(distributions=object@f, n=n, default=1, compartmentNo=compartmentNo)
  lag <- sampleTrtDistributions(distributions=object@lag, n=n, default=0, compartmentNo=compartmentNo)
  rate <- sampleTrtDistributions(distributions=object@rate, n=n, default=as.numeric(NA), compartmentNo=compartmentNo)
  duration <- sampleTrtDistributions(distributions=object@duration, n=n, default=as.numeric(NA), compartmentNo=compartmentNo)
  
  # Default infusion type:
  #   0: bolus (see above)
  #  -1: rate via dataset
  #  -2: duration via dataset
  # -99: rate/duration specified by -1 or -2 in RATE (see method apply_compartment_characteristics)
  infusionType <- ifelse(!is.na(duration), -2, NA)
  infusionType <- ifelse(!is.na(rate), -1, infusionType)
  infusionType <- ifelse(is.na(infusionType), -99, infusionType)

  retValue <- tibble::tibble(
    ID=rep(as.integer(ids), each=length(depotCmt)), ARM=as.integer(armID), TIME=object@time+lag, 
    EVID=as.integer(1), MDV=as.integer(1), AMT=object@amount*f, CMT=rep(depotCmt, length(ids)), RATE=rate, DURATION=duration,
    DOSENO=object@dose_number, INFUSION_TYPE=as.integer(infusionType), EVENT_RELATED=as.integer(FALSE)
  )
  
  # Duration or rate
  retValue <- retValue %>%
    dplyr::mutate(RATE=ifelse(.data$INFUSION_TYPE==-2, .data$AMT/.data$DURATION, .data$RATE)) %>%
    dplyr::select(-"DURATION")

  if (needsDV) {
    retValue <- retValue %>% tibble::add_column(DV=as.numeric(0), .before="INFUSION_TYPE")
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                          unwrap_treatment                             ----
#_______________________________________________________________________________

unwrap_treatment_delegate <- function(object, type) {
  time <- object@time
  ii <- object@ii
  addl <- object@addl
  
  args <- list(amount=object@amount, compartment=object@compartment,
               f=object@f, lag=object@lag, ref=object@ref)
  if (type=="infusion") {
    args$duration <- object@duration
    args$rate <- object@rate
  }
  
  if (time %>% length() > 1) {
    retValue <- time %>% 
      purrr::map(~do.call("new", c(type, list(time=.x), args)))
  } else {
    # When addl is NA, ii is also NA (see checkIIandADDL method)
    if (is.na(addl)) {
      addl <- 0
      ii <- 0
    }
    retValue <- (seq_len(addl + 1) - 1) %>%
      purrr::map(~do.call("new", c(type, list(time=time + ii*.x), args)))
  }
  return(retValue)
}

unwrap_treatmentBase <- function(object, type, wrap) {
  times <- object@time %>%
    repeat_schedule(object@rep)
  retValue <- times %>%
    purrr::map(.f=function(time) {
      object@time <- time
      return(unwrap_treatment_delegate(object, type=type))
    }) %>% unlist()
  if (!wrap && length(retValue)==1) {
    return(retValue[[1]])
  }
  return(retValue)
}

#' @rdname unwrap_treatment
setMethod("unwrap_treatment", signature = c("bolus"), definition = function(object) {
  return(object)
})

#' @rdname unwrap_treatment
setMethod("unwrap_treatment", signature = c("infusion"), definition = function(object) {
  return(object)
})

#' @rdname unwrap_treatment
setMethod("unwrap_treatment", signature = c("bolus_wrapper"), definition = function(object) {
  return(unwrap_treatmentBase(object=object, type="bolus", wrap=TRUE))
})

#' @rdname unwrap_treatment
setMethod("unwrap_treatment", signature = c("infusion_wrapper"), definition = function(object) {
  return(unwrap_treatmentBase(object=object, type="infusion", wrap=TRUE))
})

#_______________________________________________________________________________
#----                            update_amount                              ----
#_______________________________________________________________________________

update_amountDelegate <- function(object, amount, ref) {
  if (is.na(ref) || ref==object@ref) {
    object@amount <- amount
  }
  return(object)
}

#' @rdname update_amount
setMethod("update_amount", signature = c("bolus", "numeric", "character"), definition = function(object, amount, ref) {
  return(update_amountDelegate(object, amount, ref))
})

#' @rdname update_amount
setMethod("update_amount", signature = c("infusion", "numeric", "character"), definition = function(object, amount, ref) {
  return(update_amountDelegate(object, amount, ref))
})

#' @rdname update_amount
setMethod("update_amount", signature = c("bolus_wrapper", "numeric", "character"), definition = function(object, amount, ref) {
  return(update_amountDelegate(object, amount, ref))
})

#' @rdname update_amount
setMethod("update_amount", signature = c("infusion_wrapper", "numeric", "character"), definition = function(object, amount, ref) {
  return(update_amountDelegate(object, amount, ref))
})

#_______________________________________________________________________________
#----                              update_ii                                ----
#_______________________________________________________________________________

update_iiDelegate <- function(object, ii, ref) {
  if (is.na(ref) || ref==object@ref) {
    object@ii <- ii
  }
  return(object)
}

#' @rdname update_ii
setMethod("update_ii", signature = c("bolus_wrapper", "numeric", "character"), definition = function(object, ii, ref) {
  return(update_iiDelegate(object, ii, ref))
})

#' @rdname update_ii
setMethod("update_ii", signature = c("infusion_wrapper", "numeric", "character"), definition = function(object, ii, ref) {
  return(update_iiDelegate(object, ii, ref))
})

#' @rdname update_ii
setMethod("update_ii", signature = c("bolus", "numeric", "character"), definition = function(object, ii, ref) {
  return(object) # Do nothing
})

#' @rdname update_ii
setMethod("update_ii", signature = c("infusion", "numeric", "character"), definition = function(object, ii, ref) {
  return(object) # Do nothing
})

#_______________________________________________________________________________
#----                             update_addl                               ----
#_______________________________________________________________________________

update_addlDelegate <- function(object, addl, ref) {
  if (is.na(ref) || ref==object@ref) {
    object@addl <- addl
  }
  return(object)
}

#' @rdname update_addl
setMethod("update_addl", signature = c("bolus_wrapper", "integer", "character"), definition = function(object, addl, ref) {
  return(update_addlDelegate(object, addl, ref))
})

#' @rdname update_addl
setMethod("update_addl", signature = c("infusion_wrapper", "integer", "character"), definition = function(object, addl, ref) {
  return(update_addlDelegate(object, addl, ref))
})

#' @rdname update_addl
setMethod("update_addl", signature = c("bolus", "integer", "character"), definition = function(object, addl, ref) {
  return(object) # Do nothing
})

#' @rdname update_addl
setMethod("update_addl", signature = c("infusion", "integer", "character"), definition = function(object, addl, ref) {
  return(object) # Do nothing
})

#_______________________________________________________________________________
#----                             update_repeat                              ----
#_______________________________________________________________________________

update_repeatDelegate <- function(object, rep, ref) {
  if (is.na(ref) || ref==object@ref) {
    object@rep <- rep
  }
  return(object)
}

#' @rdname update_repeat
setMethod("update_repeat", signature = c("bolus_wrapper", "repeated_schedule", "character"), definition = function(object, rep, ref) {
  return(update_repeatDelegate(object, rep, ref))
})

#' @rdname update_repeat
setMethod("update_repeat", signature = c("infusion_wrapper", "repeated_schedule", "character"), definition = function(object, rep, ref) {
  return(update_repeatDelegate(object, rep, ref))
})

#' @rdname update_repeat
setMethod("update_repeat", signature = c("bolus", "repeated_schedule", "character"), definition = function(object, rep, ref) {
  return(object) # Do nothing
})

#' @rdname update_repeat
setMethod("update_repeat", signature = c("infusion", "repeated_schedule", "character"), definition = function(object, rep, ref) {
  return(object) # Do nothing
})



