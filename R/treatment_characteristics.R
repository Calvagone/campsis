
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
#----                                 select                                ----
#_______________________________________________________________________________

setMethod("select", signature=c("treatment_characteristics"), definition=function(object, ...) {
  args <- list(...)
  msg <- "Please select one of those treatment characteristics: 'infusion_duration', 'lag_time' or 'bioavailability'"
  assertthat::assert_that(length(args) > 0, msg=msg)
  type <- args[[1]]
  assertthat::assert_that(type %in% c("infusion_duration", "lag_time", "bioavailability"), msg=msg)
  
  object@list <- object@list %>% purrr::keep(~as.character(class(.x))==type)
  return(object)
})