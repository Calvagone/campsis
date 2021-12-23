#_______________________________________________________________________________
#----                       observations_set class                          ----
#_______________________________________________________________________________

#' 
#' Observations set class.
#' 
#' @export
setClass(
  "observations_set",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="observations")
)

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' @rdname getTimes
setMethod("getTimes", signature = c("observations_set"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x@times) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("observations_set"), definition=function(object) {
  times <- object %>% getTimes()
  cat(paste0("-> Obs. times: ", paste0(times, collapse=","), " (",
             times %>% length() , " observations in total)"))
})
