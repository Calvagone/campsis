
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