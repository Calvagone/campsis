
#_______________________________________________________________________________
#----                         lag_times class                                ----
#_______________________________________________________________________________

#' @export
setClass(
  "lag_times",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="lag_time") 
)

#_______________________________________________________________________________
#----                         getByCompartment                              ----
#_______________________________________________________________________________

setMethod("getByCompartment", signature = c("lag_times", "integer"), definition = function(object, compartment) {
  element <- object@list %>% purrr::keep(~(.x@compartment==compartment))
  if (length(element) > 0) {
    element <- element[[1]]
  }
  return(element)
})