
#_______________________________________________________________________________
#----                         lag_time class                                ----
#_______________________________________________________________________________

checkLagTime <- function(object) {
  return(expectOneForAll(object, c("compartment", "distribution")))
}

#' @export
setClass(
  "lag_time",
  representation(
    compartment = "integer",
    distribution = "distribution"
  ),
  contains = "pmx_element",
  validity=checkLagTime
)

#'
#' Create a lag time for the specified compartment.
#'
#' @param compartment compartment number
#' @param distribution distribution
#' @return lag time
#' @export
LagTime <- function(compartment, distribution) {
  return(new("lag_time", compartment=as.integer(compartment), distribution=distribution))
}

setMethod("getName", signature = c("lag_time"), definition = function(x) {
  return(paste0("LAG_TIME [", "CMT=", x@compartment, "]"))
})