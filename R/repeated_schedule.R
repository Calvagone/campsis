#_______________________________________________________________________________
#----                       repeated_schedule class                         ----
#_______________________________________________________________________________

#'
#' Repeated schedule class. See this class as an interface.
#'
#' @export
setClass(
  "repeated_schedule",
  representation(),
  contains = "pmx_element",
  validity = function(object) {
    TRUE
  }
)

#_______________________________________________________________________________
#----                       undefined_schedule class                        ----
#_______________________________________________________________________________

#'
#' Undefined schedule class.
#'
#' @export
setClass(
  "undefined_schedule",
  representation(),
  contains = "repeated_schedule"
)

#_______________________________________________________________________________
#----                         dosing_schedule class                         ----
#_______________________________________________________________________________

#'
#' Dosing schedule class.
#'
#' @export
setClass(
  "dosing_schedule",
  representation(),
  contains = "repeated_schedule"
)

#' Dosing schedule constructor.
#'
#' @return a dosing schedule (schedule repeated at dose times)
#' @export
DosingSchedule <- function() {
  return(new("dosing_schedule"))
}

#_______________________________________________________________________________
#----                         cyclic_schedule class                         ----
#_______________________________________________________________________________

validate_cyclic_schedule <- function(object) {
  return(expect_one_for_all(object, c("duration", "repetitions")))
}

#'
#' Cyclic schedule class.
#'
#' @slot duration duration of the cycle, numeric value
#' @slot repetitions number of additional repetitions to the base pattern, integer value
#' @export
setClass(
  "cyclic_schedule",
  representation(
    duration = "numeric",
    repetitions = "integer"
  ),
  contains = "repeated_schedule",
  validity = validate_cyclic_schedule
)

#' Cyclic schedule constructor.
#'
#' @param duration duration of the cycle, numeric value
#' @param repetitions number of additional repetitions to the base pattern, integer value
#' @return a cyclic schedule
#' @export
CyclicSchedule <- function(duration, repetitions) {
  return(new("cyclic_schedule", duration = duration, repetitions = as.integer(repetitions)))
}

#_______________________________________________________________________________
#----                       repeat_at_schedule class                        ----
#_______________________________________________________________________________

validateRepeatAtSchedule <- function(object) {
  return(expect_one_or_more(object, c("times")))
}

#'
#' 'Repeat at' schedule class.
#'
#' @slot times times at which the event is repeated, numeric vector
#' @export
setClass(
  "repeat_at_schedule",
  representation(
    times = "numeric"
  ),
  contains = "repeated_schedule",
  validity = validateRepeatAtSchedule
)

#' 'Repeat at' schedule constructor. Note that the time 0 for the base pattern
#'  will be added by default if not provided.
#'
#' @param times times at which the original schedule must be repeated, numeric vector
#' @return a 'repeat-at' schedule
#' @export
RepeatAtSchedule <- function(times) {
  times_ <- unique(c(0, times))
  return(new("repeat_at_schedule", times = times_))
}

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the number of repetition cycles.
#'
#' @param x schedule object
#' @return a number
setMethod("length", signature = c("cyclic_schedule"), definition = function(x) {
  return(x@repetitions)
})

#' Return the number of repetition cycles.
#'
#' @param x schedule object
#' @return a number
setMethod("length", signature = c("repeat_at_schedule"), definition = function(x) {
  return(length(x@times))
})

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("cyclic_schedule", "json_element"), definition = function(object, json) {
  return(campsismod::map_json_properties_to_s4_slots(object, json))
})

#_______________________________________________________________________________
#----                           repeat_schedule                               ----
#_______________________________________________________________________________

#' @rdname repeat_schedule
setMethod("repeat_schedule", signature = c("numeric", "cyclic_schedule"), definition = function(x, schedule) {
  # +1 since repetitions do not count the base pattern
  times <- seq_len(length(schedule) + 1) %>%
    purrr::map(~ x + (.x - 1) * schedule@duration) %>%
    purrr::list_c()
  return(times)
})

#' @rdname repeat_schedule
setMethod("repeat_schedule", signature = c("numeric", "repeat_at_schedule"), definition = function(x, schedule) {
  times <- seq_len(length(schedule)) %>%
    purrr::map(~ x + schedule@times[.x]) %>%
    purrr::list_c()
  return(times)
})

#' @rdname repeat_schedule
setMethod("repeat_schedule", signature = c("numeric", "undefined_schedule"), definition = function(x, schedule) {
  return(x)
})
