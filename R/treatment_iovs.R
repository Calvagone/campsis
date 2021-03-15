
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
#----                      hasModelDistribution                         ----
#_______________________________________________________________________________

setMethod("hasModelDistribution", signature = c("treatment_iovs"), definition = function(object) {
  return(object@list %>% purrr::keep(~(is(.x@distribution, "model_distribution"))) %>% length() > 0)
})
