
#_______________________________________________________________________________
#----                        dose_adaptation class                          ----
#_______________________________________________________________________________

checkDoseAdaptation <- function(object) {
  check1 <- expectOne(object, "formula")
  check2 <- expectZeroOrMore(object, "compartments")
  check3 <- expectPositiveValues(object, "compartments")
  return(c(check1, check2, check3))
}

#' 
#' Dose adaptation class.
#' 
#' @slot formula formula to apply, single character string, e.g. "AMT*WT"
#' @slot compartments compartment numbers where the formula needs to be applied
#' @export
setClass(
  "dose_adaptation",
  representation(
    formula = "character",
    compartments = "integer"
  ),
  contains="pmx_element",
  validity=checkDoseAdaptation
)

setMethod("getName", signature = c("dose_adaptation"), definition = function(x) {
  return(paste0("DOSE ADAPTATION [", "FORMULA=", x@formula, ", CMTS={", paste0(x@compartments, collapse=","), "}", "]"))
})

#'
#' Create a dose adaptation.
#'
#' @param formula formula to apply, single character string, e.g. "AMT*WT"
#' @param compartments compartment numbers where the formula needs to be applied,
#'  integer vector. Default is integer(0) (formula applied on all compartments)
#' @return a fixed covariate
#' @export
DoseAdaptation <- function(formula, compartments=integer(0)) {
  return(new("dose_adaptation", formula=formula, compartments=as.integer(compartments)))
}