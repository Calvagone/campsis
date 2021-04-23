
#_______________________________________________________________________________
#----                         treatment_iovs class                          ----
#_______________________________________________________________________________

#' @export
setClass(
  "treatment_iovs",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="treatment_iov")
)
