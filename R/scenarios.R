
#_______________________________________________________________________________
#----                         scenarios class                               ----
#_______________________________________________________________________________

#' 
#' Scenarios class.
#' 
#' @export
setClass(
  "scenarios",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="scenario") 
)

#' 
#' Create a list of scenarios.
#' 
#' @return a scenarios object
#' @export
Scenarios <- function() {
  return(new("scenarios"))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("scenarios", "scenario"), definition = function(object, x) {
  # Create default name is name was not set
  if (is.na(x@name)) {
    x@name <- paste("Scenario", object %>% length() + 1)
  } 
  return(callNextMethod(object, x))
})