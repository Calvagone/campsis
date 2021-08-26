#_______________________________________________________________________________
#----                        dose_adaptations class                         ----
#_______________________________________________________________________________

#' 
#' Dose adaptations class.
#' 
#' @export
setClass(
  "dose_adaptations",
  representation(
  ),
  contains = "pmx_list",
  prototype = prototype(type="dose_adaptation") 
)