
#_______________________________________________________________________________
#----                           events class                                ----
#_______________________________________________________________________________

#' @export
setClass(
  "events",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="event") 
)

#' 
#' Create a list of interruption events.
#' 
#' @return a events object
#' @export
Events <- function() {
  return(new("events"))
}

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' @rdname getTimes
setMethod("getTimes", signature = c("events"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x@times) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})