
#_______________________________________________________________________________
#----                     observation class                                ----
#_______________________________________________________________________________

checkObservation <- function(object) {
  check1 <- checkObject(object, c("compartment"))
  check2 <- checkArms(object)
  return(checkReturn(c(check1, check2)))
}

#' 
#' Observation entry class.
#' 
#' @export
setClass(
  "observation",
  representation(
    compartment = "integer",
    arms = "list"
  ),
  contains = "dataset_entry",
  prototype=prototype(compartment=as.integer(NA), arms=list()),
  validity=checkObservation
)
