
#_______________________________________________________________________________
#----                           event class                                 ----
#_______________________________________________________________________________

checkEvent <- function(object) {
  checkName <- expectOne(object, "name")
  checkTimes <- expectOneOrMore(times, "times")
  checkFunction <- expectOne(object, "fun")
  checkDebug <- expectOne(object, "debug")
  return(c(checkName, checkTimes, checkFunction, checkDebug))
}

#' @export
setClass(
  "event",
  representation(
    name = "character",
    times = "numeric",
    fun = "function",
    debug = "logical"
  ),
  contains="pmx_element",
  prototype=prototype(name="", debug=FALSE)
)

#' 
#' Create an interruption event.
#' 
#' @param name event name, character value
#' @param times interruption times, numeric vector
#' @param fun event function to apply at each interrution
#' @param debug output the variables that were changed through this event
#' @return an event definition
#' @export
Event <- function(name=NULL, times, fun, debug=FALSE) {
  if (is.null(name)) {
    name <- "Unnamed event"
  }
  return(new("event", name=name, times=times, fun=fun, debug=debug))
}

#_______________________________________________________________________________
#----                           getName                                     ----
#_______________________________________________________________________________

setMethod("getName", signature = c("event"), definition = function(x) {
  return(paste0("EVENT (", x@name, ")"))
})
