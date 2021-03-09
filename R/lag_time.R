
#_______________________________________________________________________________
#----                         lag_time class                                ----
#_______________________________________________________________________________

checkLagTime <- function(object) {
  return(expectOneForAll(object, c("compartment", "mean", "variance")))
}

#' @export
setClass(
  "lag_time",
  representation(
    compartment = "integer",
    mean = "numeric",
    variance = "numeric" 
  ),
  contains = "pmx_element",
  validity=checkLagTime
)

#'
#' Create a lag time for the specified compartment.
#'
#' @param compartment compartment number
#' @param mean lag time mean value
#' @param variance lag time variance
#' @return lag time
#' @export
LagTime <- function(compartment, mean, variance) {
  return(new("lag_time", compartment=as.integer(compartment), mean=as.numeric(mean), variance=as.numeric(variance)))
}

setMethod("getName", signature = c("lag_time"), definition = function(x) {
  return(paste0("LAG_TIME [", "CMT=", x@compartment, "]"))
})