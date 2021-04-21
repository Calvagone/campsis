#_______________________________________________________________________________
#----                       observations_set class                          ----
#_______________________________________________________________________________

#' @export
setClass(
  "observations_set",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="observations")
)
