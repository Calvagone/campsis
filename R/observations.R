
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

setMethod("sample", signature = c("observations", "integer"), definition = function(object, n, ...) {
  args <- list(...)
  config <- processExtraArg(args, name="config", mandatory=TRUE)
  maxID <- processExtraArg(args, name="maxID", mandatory=TRUE)
  ids <- seq_len(subjects) + maxID - subjects
  
  if (is.na(object@compartment)) {
    obsCmt <- config@def_obs_cmt
  } else {
    obsCmt <- object@compartment
  }
  return(data.frame(ID=rep(ids, each=length(object@times)), TIME=rep(object@times, n), EVID=as.integer(0), MDV=as.integer(0),
                    AMT=as.numeric(NA), CMT=obsCmt, DOSENO=as.integer(NA), IS_INFUSION=as.logical(NA)))
})
