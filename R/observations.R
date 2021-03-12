#_______________________________________________________________________________
#----                        observations class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "observations",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="observation") 
)

#' 
#' Create a list of observations.
#' 
#' @param time observation times, numeric vector
#' @param compartment compartment index, integer
#' @return a list of observations, linked to the same compartment number
#' @export
Observations <- function(times, compartment=NA) {
  retValue <- new("observations")
  for (time in times) {
    retValue <- retValue %>% pmxmod::add(Observation(time=time, compartment=compartment))
  }
  return(retValue)
}

