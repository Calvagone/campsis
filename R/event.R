#_______________________________________________________________________________
#----                           event class                                 ----
#_______________________________________________________________________________

check_event <- function(object) {
  checkName <- expect_one(object, "name")
  checkTimes <- expect_one_or_more(object, "times")
  checkTimesPositive <- expect_positive_values(object, "times")
  checkFunction <- expect_one(object, "fun")
  checkDebug <- expect_one(object, "debug")
  return(c(checkName, checkTimes, checkTimesPositive, checkFunction, checkDebug))
}

#'
#' Event class.
#'
#' @slot name event name, character value
#' @slot times interruption times, numeric vector
#' @slot fun event function to apply at each interruption
#' @slot debug output the variables that were changed through this event
#' @export
setClass(
  "event",
  representation(
    name = "character",
    times = "numeric",
    fun = "function",
    debug = "logical"
  ),
  contains = "pmx_element",
  prototype = prototype(name = "", debug = FALSE),
  validity = check_event
)

#'
#' Create an interruption event.
#'
#' @param name event name, character value
#' @param times interruption times, numeric vector
#' @param fun event function to apply at each interruption
#' @param debug output the variables that were changed through this event
#' @return an event definition
#' @export
Event <- function(name = NULL, times, fun, debug = FALSE) {
  if (is.null(name)) {
    name <- "Unnamed event"
  }
  return(new("event", name = name, times = times, fun = fun, debug = debug))
}

#_______________________________________________________________________________
#----                           get_name                                     ----
#_______________________________________________________________________________

setMethod("get_name", signature = c("event"), definition = function(x) {
  return(paste0("EVENT (", x@name, ")"))
})
