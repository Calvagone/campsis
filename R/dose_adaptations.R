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

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("dose_adaptations"), definition=function(object) {
  doseAdaptations <- object@list
  for (index in seq_len(doseAdaptations %>% length())) {
    show(doseAdaptations[[index]])
    cat("\n")
  }
})
