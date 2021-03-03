
#_______________________________________________________________________________
#----                        covariates class                               ----
#_______________________________________________________________________________

#' 
#' Covariates class.
#' 
#' @export
setClass(
  "covariates",
  representation(
    list = "list"
  ),
  prototype=prototype(list=list())
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

setMethod("add", signature=c("covariates", "covariate"), definition=function(object, x) {
  if (validObject(x)) {
    if (object %>% hasElement(x)) {
      stop(paste("Covariate", x@name, "is already present."))
    } else {
      object@list <- c(object@list, x)
    }
  }
  return(object)
})

#_______________________________________________________________________________
#----                            hasElement                                 ----
#_______________________________________________________________________________

setMethod("hasElement", signature=c("covariates", "character"), definition=function(object, x) {
  return(x %in% (object %>% getNames()))
})

setMethod("hasElement", signature=c("covariates", "covariate"), definition=function(object, x) {
  return(object %>% hasElement(x@name))
})

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

setMethod("getNames", signature=c("covariates"), definition=function(object) {
  return(object@list %>% purrr::map_chr(.f=~.x@name))
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("covariates"), definition=function(x) {
  return(length(x@list))
})
