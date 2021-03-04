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
#----                                  sort                                 ----
#_______________________________________________________________________________

setMethod("sort", signature=c("time_entries"), definition=function(x, decreasing=FALSE, ...) {
  types <- x@list %>% purrr::map_chr(~as.character(class(.x)))
  times <- x@list %>% purrr::map_dbl(~.x@time)
  
  # Reorder
  types <- factor(types, levels=c("observation", "bolus", "infusion"), labels=c("observation", "bolus", "infusion"))
  order <- order(times, types)
  
  # Apply result to original list
  x@list <- x@list[order]
  return(x)
})