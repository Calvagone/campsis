
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
  ),
  contains = "pmx_list",
  prototype = prototype(type="covariate") 
)

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

setMethod("getNames", signature=c("covariates"), definition=function(object) {
  return(object@list %>% purrr::map_chr(.f=~.x@name))
})
