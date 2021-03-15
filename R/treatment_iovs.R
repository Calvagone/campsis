
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

#_______________________________________________________________________________
#----                      hasParameterDistribution                         ----
#_______________________________________________________________________________

setMethod("hasParameterDistribution", signature = c("treatment_iovs"), definition = function(object) {
  return(object@list %>% purrr::keep(~(is(.x@distribution, "parameter_distribution"))) %>% length() > 0)
})