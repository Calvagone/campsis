
#_______________________________________________________________________________
#----                        arms class                               ----
#_______________________________________________________________________________

#' 
#' Arms class.
#' 
#' @export
setClass(
  "arms",
  representation(
  ),
  contains="pmx_list"
)


#_______________________________________________________________________________
#----                              default                                  ----
#_______________________________________________________________________________

setMethod("default", signature=c("arms"), definition=function(object, ...) {
  if (object %>% length() == 0) {
    arm = new("arm", id=as.integer(0))
    object <- object %>% add(arm)
  }
  return(object@list[[1]])
})

