#_______________________________________________________________________________
#----                           events class                                ----
#_______________________________________________________________________________

#'
#' Events class.
#'
#' @export
setClass(
  "events",
  representation(),
  contains = "pmx_list",
  prototype = prototype(type = "event")
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
#----                             get_times                                 ----
#_______________________________________________________________________________

#' @rdname get_times
setMethod("get_times", signature = c("events"), definition = function(object) {
  return(object@list %>% purrr::map(.f = ~ as.numeric(.x@times)) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})
