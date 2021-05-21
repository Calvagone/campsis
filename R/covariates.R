
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

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

setMethod("select", signature=c("covariates"), definition=function(object, ...) {
  args <- list(...)
  type <- args[[1]]
  selection <- c("time_varying_covariate")
  msg <- paste0("Only this type can be selected: ", paste0("'", selection, "'", collapse=", "))
  assertthat::assert_that(type %>% length() == 1 && type %in% selection, msg=msg)
  object@list <- object@list %>% purrr::keep(~is(.x, type))
  return(object)
})
