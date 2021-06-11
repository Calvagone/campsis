
#_______________________________________________________________________________
#----                           occasions class                             ----
#_______________________________________________________________________________

#' @export
setClass(
  "occasions",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="occasion")
)

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

setMethod("getNames", signature=c("occasions"), definition=function(object) {
  return(object@list %>% purrr::map_chr(.f=~.x@colname))
})
