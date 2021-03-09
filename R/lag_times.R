
#_______________________________________________________________________________
#----                         lag_times class                                ----
#_______________________________________________________________________________

#' @export
setClass(
  "lag_times",
  representation(
  ),
  contains="pmx_list"
)

#_______________________________________________________________________________
#----                         getByCompartment                              ----
#_______________________________________________________________________________

#' Get element by compartment number.
#' 
#' @param object any object
#' @param compartment compartment number
#' @return the element that has the right compartment
#' @export
getByCompartment <- function(object, compartment) {
  stop("No default function is provided")
}

setGeneric("getByCompartment", function(object, compartment) {
  standardGeneric("getByCompartment")
})

setMethod("getByCompartment", signature = c("lag_times", "integer"), definition = function(object, compartment) {
  element <- object@list %>% purrr::keep(~(.x@compartment==compartment))
  if (length(element) > 0) {
    element <- element[[1]]
  }
  return(element)
})