
#_______________________________________________________________________________
#----                            arms class                                 ----
#_______________________________________________________________________________

#' 
#' Arms class.
#' 
#' @export
setClass(
  "arms",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="arm") 
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

#_______________________________________________________________________________
#----                          getCovariateNames                            ----
#_______________________________________________________________________________

setMethod("getCovariateNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getCovariateNames()) %>% purrr::flatten_chr() %>% unique())
})

#_______________________________________________________________________________
#----                            getIOVNames                                ----
#_______________________________________________________________________________

setMethod("getIOVNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getIOVNames())  %>% purrr::flatten_chr() %>% unique())
})


#_______________________________________________________________________________
#----                         getOccasionNames                              ----
#_______________________________________________________________________________

setMethod("getOccasionNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getOccasionNames())  %>% purrr::flatten_chr() %>% unique())
})

#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

setMethod("getTimeVaryingCovariateNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getTimeVaryingCovariateNames()) %>% purrr::flatten_chr() %>% unique())
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

setMethod("getTimes", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getTimes()) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})

