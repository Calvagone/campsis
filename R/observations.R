
#_______________________________________________________________________________
#----                     observations class                                ----
#_______________________________________________________________________________

check_observations <- function(object) {
  times <- get_times(object)
  check1 <- expect_one(object, "compartment")
  check2 <- character()
  if (object@dv %>% length() > 0 && object@dv %>% length() != length(times)) {
    check2 <- "Slots 'times' and dv' don't have the same length"
  }
  
  check3 <- expect_one_or_more_(times, "times")
  check4 <- expect_positive_values_(times, "times")

  return(c(check1, check2, check3, check4))
}

#' 
#' Observations class.
#' 
#' @slot times any object that implements 
#' @slot compartment compartment index (integer) or name (character)
#' @slot dv observed values, numeric vector (FOR EXTERNAL USE)
#' @slot rep repetition schedule
#' @export
setClass(
  "observations",
  representation(
    times = "time_vector",
    compartment = "character",
    rep = "repeated_schedule",
    dv="numeric"
  ),
  contains = "pmx_element",
  prototype = prototype(compartment=as.character(NA), dv=numeric(0), rep=new("undefined_schedule")),
  validity = check_observations
)

#'
#' Create an observations list. Please note that the provided 'times' will 
#' automatically be sorted. Duplicated times will be removed.
#'
#' @param times observation times, numeric vector
#' @param compartment compartment index (integer) or name (character)
#' @param rep repetition schedule
#' @return an observations list
#' @export
Observations <- function(times, compartment=NA, rep=NULL) {
  if (is(times, "time_vector")) {
    # Do nothing
  } else {
    times <- TimeVector(times)
  }
  if (is.null(rep)) {
    rep <- new("undefined_schedule")
  }
  return(new("observations", times=times, compartment=as.character(compartment), rep=rep))
}

setMethod("get_name", signature = c("observations"), definition = function(x) {
  return(paste0("OBS [", "TIMES=c(", paste0(get_times(x), collapse=","), "), ", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                     event_related_observations class                  ----
#_______________________________________________________________________________

setClass(
  "event_related_observations",
  representation(
  ),
  contains = "observations"
)

#'
#' Create an event-related observations list. Please note that the provided 'times' will 
#' automatically be sorted. Duplicated times will be removed.
#'
#' @param times observation times, numeric vector
#' @param compartment compartment index, integer
#' @return observations
#' @keywords internal
EventRelatedObservations <- function(times, compartment=NA) {
  return(new("event_related_observations", times=TimeVector(times), compartment=as.character(compartment)))
}

#_______________________________________________________________________________
#----                             get_times                                 ----
#_______________________________________________________________________________

#' @param doseTimes times of the doses, only needed if a [DosingSchedule()] is referred to
#' @rdname get_times
setMethod("get_times", signature = c("observations"), definition = function(object, doseTimes=NULL) {
  times <- as.numeric(object@times)
  rep <- object@rep
  if (is(rep, "dosing_schedule")) {
    rep <- RepeatAtSchedule(doseTimes)
  }
  times_ <- times %>%
    repeat_schedule(rep)
  
  return(base::sort(unique(times_)))
})

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("observations", "json_element"), definition=function(object, json) {
  if (is.numeric(unlist(json@data$times))) {
    object@times <- TimeVector(unlist(json@data$times))
    json@data$times <- NULL
  }
  # Retrieving time unit in JSON
  unit <- "hour" # Default
  if (!is.null(json@data$unit)) {
    unit <- json@data$unit
    json@data$unit <- NULL
  }
  object <- campsismod::map_json_properties_to_s4_slots(object, json)
  
  # Handling time unit
  if (is(object@times, "time_sequence")) {
    object@times@start <- convert_time(object@times@start, from=unit, to="hour")
    object@times@end <- convert_time(object@times@end, from=unit, to="hour")
    object@times@by <- convert_time(object@times@by, from=unit, to="hour")
  } else if (is(object@times, "time_vector")) {
      object@times@.Data <- convert_time(object@times@.Data, from=unit, to="hour")
  } else {
    stop("Either a 'time_vector' or a 'time_sequence'")
  }
  return(object)
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' @rdname sample
setMethod("sample", signature = c("observations", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- process_extra_arg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- process_extra_arg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- process_extra_arg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  needsDV <- process_extra_arg(args, name="needsDV", mandatory=TRUE, default=FALSE)
  doseTimes <- process_extra_arg(args, name="doseTimes", mandatory=TRUE, default=NULL)
  
  if (is.na(object@compartment)) {
    obsCmt <- as.character(config@def_obs_cmt)
  } else {
    obsCmt <- object@compartment
  }
  isEventRelated <- is(object, "event_related_observations")
  times <- get_times(object, doseTimes=doseTimes)
  
  retValue <- tibble::tibble(
    ID=rep(ids, each=length(times)), ARM=as.integer(armID), TIME=rep(times, n),
    EVID=as.integer(0), MDV=as.integer(0), AMT=as.numeric(NA), CMT=obsCmt, RATE=as.numeric(0), DOSENO=as.integer(NA),
    INFUSION_TYPE=as.integer(NA), EVENT_RELATED=as.integer(isEventRelated)
  )
  if (needsDV) {
    if (object@dv %>% length() > 0) {
      dv <- object@dv
    } else {
      dv <- rep(as.numeric(0),  length(times))
    }
    retValue <- retValue %>% tibble::add_column(DV=rep(dv, n), .before="INFUSION_TYPE")
  }
  return(retValue)
})
