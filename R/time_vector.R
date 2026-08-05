#_______________________________________________________________________________
#----                         Time vector interface                         ----
#_______________________________________________________________________________

#'
#' Time vector class.
#'
#' @export
setClass(
  "time_vector",
  representation(),
  contains = "numeric"
)

#'
#' Instantiate a new time vector
#'
#' @param x time vector, numeric
#' @return a time vector
#' @export
TimeVector <- function(x) {
  return(new("time_vector", base::sort(unique(x))))
}

#_______________________________________________________________________________
#----                         Time sequence class                           ----
#_______________________________________________________________________________

#'
#' Time sequence class.
#'
#' @export
setClass(
  "time_sequence",
  representation(
    start = "numeric",
    end = "numeric",
    by = "numeric"
  ),
  contains = "time_vector"
)

#'
#' Instantiate a new time sequence.
#'
#' @param start starting value
#' @param end maximal value
#' @param by increment of the sequence
#' @return a sequence
#' @export
TimeSequence <- function(start, end, by) {
  return(new("time_sequence", start = start, end = end, by = by))
}

#_______________________________________________________________________________
#----                            as.numeric                                 ----
#_______________________________________________________________________________

#' Time vector to numeric vector.
#'
#' @param x time vector object
#' @return a numeric vector
#'
setMethod("as.numeric", signature = c("time_vector"), definition = function(x) {
  return(base::sort(unique(x)))
})

#' Time sequence to numeric vector.
#'
#' @param x time sequence object
#' @return a numeric vector
#'
setMethod("as.numeric", signature = c("time_sequence"), definition = function(x) {
  return(seq(from = x@start, to = x@end, by = x@by))
})

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("time_sequence", "json_element"), definition = function(object, json) {
  object <- campsismod::map_json_properties_to_s4_slots(object, json)
  return(object)
})
