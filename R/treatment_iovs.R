
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

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("treatment_iovs"), definition=function(object) {
  if (object %>% length() > 0) {
    cat("-> Treatment IOV:", paste0(object %>% getNames(), collapse=","))
    cat("\n")
  }
})
