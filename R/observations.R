#_______________________________________________________________________________
#----                        observations class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "observations",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="observation") 
)

