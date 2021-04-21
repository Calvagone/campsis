
#_______________________________________________________________________________
#----                     observations class                                ----
#_______________________________________________________________________________

checkObservations <- function(object) {
  check1 <- expectOneOrMore(object, "times")
  check2 <- expectOne(object, "compartment")
  return(c(check1, check2))
}

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
#----                            convert                                    ----
#_______________________________________________________________________________

setMethod("convert", signature = c("observations"), definition = function(object, config) {
  if (is.na(object@compartment)) {
    obsCmt <- config@def_obs_cmt
  } else {
    obsCmt <- object@compartment
  }
  return(data.frame(TIME=object@times, EVID=as.integer(0), MDV=as.integer(0),
                    AMT=as.numeric(NA), CMT=obsCmt, DOSENO=as.integer(NA), IS_INFUSION=as.logical(NA)))
})
