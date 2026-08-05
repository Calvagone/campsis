#_______________________________________________________________________________
#----                       observations_set class                          ----
#_______________________________________________________________________________

#'
#' Observations set class.
#'
#' @export
setClass(
  "observations_set",
  representation(),
  contains = "pmx_list",
  prototype = prototype(type = "observations")
)

#_______________________________________________________________________________
#----                             get_times                                 ----
#_______________________________________________________________________________

#' @param doseTimes times of the doses, only needed if a [DosingSchedule()] is referred to
#' @rdname get_times
setMethod("get_times", signature = c("observations_set"), definition = function(object, doseTimes = NULL) {
  times <- object@list %>%
    purrr::map(.f = ~ get_times(.x, doseTimes = doseTimes)) %>%
    purrr::flatten_dbl()
  return(base::sort(unique(times)))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("observations_set"), definition = function(object) {
  times <- get_times(object, doseTimes = NULL)
  cat(paste0("-> Obs. times: ", paste0(times, collapse = ","), " (", times %>% length(), " observations in total)"))
})
