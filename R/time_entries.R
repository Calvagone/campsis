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
  ),
  contains="pmx_list"
)

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