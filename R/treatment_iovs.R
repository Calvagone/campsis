
#_______________________________________________________________________________
#----                         treatment_iovs class                          ----
#_______________________________________________________________________________

#' 
#' Treatment IOV's class.
#' 
#' @export
setClass(
  "treatment_iovs",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="treatment_iov")
)
