
#_______________________________________________________________________________
#----                   treatment_characteristics class                     ----
#_______________________________________________________________________________

#' @export
setClass(
  "treatment_characteristics",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="treatment_characteristic") 
)

#_______________________________________________________________________________
#----                         getByCompartment                              ----
#_______________________________________________________________________________

setMethod("getByCompartment", signature = c("treatment_characteristics", "integer"), definition = function(object, compartment) {
  element <- object@list %>% purrr::keep(~(.x@compartment==compartment))
  if (length(element) > 0) {
    element <- element[[1]]
  }
  return(element)
})

#_______________________________________________________________________________
#----                      hasModelDistribution                         ----
#_______________________________________________________________________________

setMethod("hasModelDistribution", signature = c("treatment_characteristics"), definition = function(object) {
  return(object@list %>% purrr::keep(~(is(.x@distribution, "model_distribution"))) %>% length() > 0)
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

setMethod("select", signature=c("treatment_characteristics"), definition=function(object, ...) {
  args <- list(...)
  msg <- "Please select one of those treatment characteristics: 'treatment_infusion_duration', 'treatment_lag_time' or 'treatment_bioavailability'"
  assertthat::assert_that(length(args) > 0, msg=msg)
  type <- args[[1]]
  assertthat::assert_that(type %in% c("treatment_infusion_duration", "treatment_lag_time", "treatment_bioavailability"), msg=msg)
  
  object@list <- object@list %>% purrr::keep(~as.character(class(.x))==type)
  return(object)
})
