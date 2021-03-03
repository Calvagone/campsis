
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
    list = "list"
  ),
  prototype=prototype(list=list())
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

setMethod("add", signature=c("arms", "arm"), definition=function(object, x) {
  if (validObject(x)) {
    if (FALSE) {
      stop(paste("Arm", x@id, "is already present."))
    } else {
      object@list <- c(object@list, x)
    }
  }
  return(object)
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("arms"), definition=function(x) {
  return(length(x@list))
})
