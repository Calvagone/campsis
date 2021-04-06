#_______________________________________________________________________________
#----                   treatment_infusion_duration class                   ----
#_______________________________________________________________________________

validateInfusionDuration <- function(object) {
  return(expectOneForAll(object, c("rate")))
}

#' @export
setClass(
  "treatment_infusion_duration",
  representation(
    rate = "logical"
  ),
  contains = "treatment_characteristic",
  prototype=prototype(rate=FALSE),
  validity=validateInfusionDuration
)

#'
#' Create an infusion duration/rate.
#'
#' @param compartment compartment number
#' @param distribution distribution of this infusion
#' @param rate logical value, TRUE if distribution is a rate, FALSE if it is an infusion
#' @return details about infusion duration/rate
#' @export
TreatmentInfusionDuration <- function(compartment, distribution, rate=FALSE) {
  return(new("treatment_infusion_duration", compartment=as.integer(compartment), distribution=distribution, rate=rate))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________

setMethod("getName", signature = c("treatment_infusion_duration"), definition = function(x) {
  return(paste0("INFUSION_DURATION [", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                         getColumnName                                 ----
#_______________________________________________________________________________

setMethod("getColumnName", signature = c("treatment_infusion_duration"), definition = function(x) {
  return(paste0("D", x@compartment))
})
