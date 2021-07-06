
#_______________________________________________________________________________
#----                     observations class                                ----
#_______________________________________________________________________________

checkObservations <- function(object) {
  check1 <- expectOneOrMore(object, "times")
  check2 <- expectPositiveTimes(object@times)
  check3 <- expectOne(object, "compartment")
  return(c(check1, check2, check3))
}

#' 
#' Observations class.
#' 
#' @slot times observation times, numeric vector
#' @slot compartment compartment index, integer
#' @export
setClass(
  "observations",
  representation(
    times = "numeric",
    compartment = "integer"
  ),
  contains = "pmx_element",
  prototype = prototype(compartment=as.integer(NA)),
  validity = checkObservations
)

#'
#' Create an observations list. Please note that the provided 'times' will 
#' automatically be sorted. Duplicated times will be removed.
#'
#' @param times observation times, numeric vector
#' @param compartment compartment index, integer
#' @return observations
#' @export
Observations <- function(times, compartment=NA) {
  return(new("observations", times=base::sort(unique(times)), compartment=as.integer(compartment)))
}

setMethod("getName", signature = c("observations"), definition = function(x) {
  return(paste0("OBS [", "TIMES=c(", paste0(x@times, collapse=","), "), ", "CMT=", x@compartment, "]"))
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
  return(new("event_related_observations", times=base::sort(unique(times)), compartment=as.integer(compartment)))
}

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' @rdname sample
setMethod("sample", signature = c("observations", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE, default=DatasetConfig())
  ids <- processExtraArg(args, name="ids", mandatory=TRUE, default=seq_len(n))
  armID <- processExtraArg(args, name="armID", mandatory=TRUE, default=as.integer(0))
  
  if (is.na(object@compartment)) {
    obsCmt <- config@def_obs_cmt
  } else {
    obsCmt <- object@compartment
  }
  isEventRelated <- is(object, "event_related_observations")
  return(tibble::tibble(ID=rep(ids, each=length(object@times)), ARM=as.integer(armID), TIME=rep(object@times, n), EVID=as.integer(0), MDV=as.integer(0),
                    AMT=as.numeric(NA), CMT=obsCmt, RATE=as.numeric(0), DOSENO=as.integer(NA), IS_INFUSION=as.logical(NA), EVENT_RELATED=as.integer(isEventRelated)))
})
