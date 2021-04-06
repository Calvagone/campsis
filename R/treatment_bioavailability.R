
#_______________________________________________________________________________
#----                     treatment_bioavailability class                   ----
#_______________________________________________________________________________

validateBioavailability <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "treatment_bioavailability",
  representation(
  ),
  contains = "treatment_characteristic",
  validity=validateBioavailability
)

#'
#' Create a bioavailability for the specified compartment.
#'
#' @param compartment compartment number
#' @param distribution distribution
#' @return bioavailability
#' @export
TreatmentBioavailability <- function(compartment, distribution) {
  return(new("treatment_bioavailability", compartment=as.integer(compartment), distribution=distribution))
}

#_______________________________________________________________________________
#----                            getName                                    ----
#_______________________________________________________________________________


setMethod("getName", signature = c("treatment_bioavailability"), definition = function(x) {
  return(paste0("BIOAVAILABILITY [", "CMT=", x@compartment, "]"))
})

#_______________________________________________________________________________
#----                         getColumnName                                 ----
#_______________________________________________________________________________

setMethod("getColumnName", signature = c("treatment_bioavailability"), definition = function(x) {
  return(paste0("F", x@compartment))
})