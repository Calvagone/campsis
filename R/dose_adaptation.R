
#_______________________________________________________________________________
#----                        dose_adaptation class                          ----
#_______________________________________________________________________________

checkDoseAdaptation <- function(object) {
  check1 <- expect_one(object, "formula")
  check2 <- expect_zero_or_more(object, "compartments")
  check3 <- expect_positive_values(object, "compartments")
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
    compartments = "character"
  ),
  contains="pmx_element",
  validity=checkDoseAdaptation
)

setMethod("get_name", signature = c("dose_adaptation"), definition = function(x) {
  return(sprintf("DOSE ADAPTATION [CMT=%s]", getDoseAdaptationCmtString(x, vector=TRUE)))
})

#'
#' Create a dose adaptation.
#'
#' @param formula formula to apply, single character string, e.g. "AMT*WT"
#' @param compartments compartment indexes or names where the formula needs to be applied,
#'  integer or character vector. Default is NULL (formula applied on all compartments)
#' @return a fixed covariate
#' @export
DoseAdaptation <- function(formula, compartments=NULL) {
  return(new("dose_adaptation", formula=formula, compartments=as.character(compartments)))
}

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("dose_adaptation", "json_element"), definition=function(object, json) {
  object <- campsismod::map_json_properties_to_s4_slots(object, json)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

getDoseAdaptationCmtString <- function(object, vector=FALSE) {
  if (object@compartments %>% length() == 0) {
    str <- "ALL"
  } else {
    str <- sprintf("%s", paste0(object@compartments, collapse=","))
    if (vector) str <- sprintf("c(%s)", str)
  }
  return(str)
}

setMethod("show", signature=c("dose_adaptation"), definition=function(object) {
  str <- sprintf("-> Dose adaptation (CMT=%s): %s", getDoseAdaptationCmtString(object), object@formula)
  cat(str)
})
