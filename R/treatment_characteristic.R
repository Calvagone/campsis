#_______________________________________________________________________________
#----                   treatment_characteristic class                      ----
#_______________________________________________________________________________

validateTreatmentCharacteristic <- function(object) {
  return(expectOneForAll(object, c("compartment", "distribution")))
}

#' 
#' Treatment characteristic class.
#' 
#' @export
setClass(
  "treatment_characteristic",
  representation(
    compartment = "integer",
    distribution = "distribution"
  ),
  contains="pmx_element",
  validity=validateTreatmentCharacteristic 
)