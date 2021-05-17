
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

#' Get all distinct times across the events list.
#' 
#' @param object any object
#' @return numeric vector with all unique times, sorted
#' @export
getTimes <- function(object) {
  stop("No default function is provided")
}

setGeneric("getTimes", function(object) {
  standardGeneric("getTimes")
})

setMethod("getTimes", signature = c("events"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x@times) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})