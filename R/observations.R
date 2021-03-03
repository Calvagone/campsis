#_______________________________________________________________________________
#----                        observations class                             ----
#_______________________________________________________________________________

#' 
#' Observations class.
#' 
#' @export
setClass(
  "observations",
  representation(
    list = "list"
  ),
  prototype=prototype(list=list())
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

setMethod("add", signature=c("observations", "observation"), definition=function(object, x) {
  if (validObject(x)) {
    object@list <- c(object@list, x)
  }
  return(object)
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("observations"), definition=function(x) {
  return(length(x@list))
})

