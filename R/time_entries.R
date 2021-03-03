#_______________________________________________________________________________
#----                        time_entries class                             ----
#_______________________________________________________________________________

#' 
#' Time_entries class.
#' 
#' @export
setClass(
  "time_entries",
  representation(
    list = "list"
  ),
  prototype=prototype(list=list())
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

setMethod("add", signature=c("time_entries", "time_entry"), definition=function(object, x) {
  if (validObject(x)) {
    object@list <- c(object@list, x)
  }
  return(object)
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("time_entries"), definition=function(x) {
  return(length(x@list))
})

#_______________________________________________________________________________
#----                                  order                                ----
#_______________________________________________________________________________

setMethod("order", signature=c("time_entries"), definition=function(object) {
  types <- object@list %>% purrr::map_chr(~as.character(class(.x)))
  times <- object@list %>% purrr::map_dbl(~.x@time)
  
  # Reorder
  types <- factor(types, levels=c("observation", "bolus", "infusion"), labels=c("observation", "bolus", "infusion"))
  order <- base::order(times, types)
  
  # Apply result to original list
  object@list <- object@list[order]
  return(object)
})