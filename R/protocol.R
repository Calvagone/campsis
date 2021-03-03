#_______________________________________________________________________________
#----                         protocol class                                ----
#_______________________________________________________________________________

#' 
#' Protocol class.
#' 
#' @export
setClass(
  "protocol",
  representation(
    treatment = "treatment",
    observations = "observations"
  ),
  prototype=prototype(treatment=new("treatment"), observations=new("observations"))
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

setMethod("add", signature=c("protocol", "treatment_entry"), definition=function(object, x) {
  return(object@treatment %>% add(x))
})

setMethod("add", signature=c("protocol", "observation"), definition=function(object, x) {
  return(object@observations %>% add(x))
})
