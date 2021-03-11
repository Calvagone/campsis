
#_______________________________________________________________________________
#----                     infusion_durations class                          ----
#_______________________________________________________________________________

#' @export
setClass(
  "infusion_durations",
  representation(
  ),
  contains="pmx_list"
)

#_______________________________________________________________________________
#----                         getByCompartment                              ----
#_______________________________________________________________________________

setMethod("getByCompartment", signature = c("infusion_durations", "integer"), definition = function(object, compartment) {
  element <- object@list %>% purrr::keep(~(.x@compartment==compartment))
  if (length(element) > 0) {
    element <- element[[1]]
  }
  return(element)
})