
#_______________________________________________________________________________
#----                           events class                                ----
#_______________________________________________________________________________

#' @export
setClass(
  "events",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="event") 
)

#' 
#' Create a list of interruption events.
#' 
#' @return a events object
#' @export
Events <- function() {
  return(new("events"))
}
