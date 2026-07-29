#' Compute the prediction interval summary over time (deprecated).
#' 
#' @param x data frame
#' @param variable variable(s) used to compute the prediction interval, character vector.
#'   When more than one variable is supplied, a \code{variable} column is added to the
#'   output to identify each one.
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @return a summary table
#' @export
#' @keywords internal
PI <- function(x, variable, strata = get_default_strata(), level = 0.90) {
  .Deprecated("compute_pi")
  return(compute_pi(x=x, variable=variable, strata=strata, level=level))  
}

#_______________________________________________________________________________
#----                           generic.R                                   ----
#_______________________________________________________________________________

#' Set the label.
#' 
#' `setLabel()` is deprecated in favor of `set_label()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams set_label
#' @return the updated object
#' @export
#' @rdname setLabel
setLabel <- function(object, x) {
  lifecycle::deprecate_warn("1.9.0", "setLabel()", "set_label()")
  set_label(object = object, x = x)
}

setGeneric("setLabel", function(object, x) {
  lifecycle::deprecate_warn("1.9.0", "setLabel()", "set_label()")
  set_label(object = object, x = x)
})

#' Set the number of subjects.
#' 
#' `setSubjects()` is deprecated in favor of `set_subjects()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams set_subjects
#' @return the updated object
#' @export
#' @rdname setSubjects
setSubjects <- function(object, x) {
  lifecycle::deprecate_warn("1.9.0", "setSubjects()", "set_subjects()")
  set_subjects(object = object, x = x)
}

setGeneric("setSubjects", function(object, x) {
  lifecycle::deprecate_warn("1.9.0", "setSubjects()", "set_subjects()")
  set_subjects(object = object, x = x)
})

#_______________________________________________________________________________
#----                        time_utilities.R                               ----
#_______________________________________________________________________________

#' Convert numeric time vector based on the provided units.
#' 
#' `convertTime()` is deprecated in favor of `convert_time()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams convert_time
#' @return numeric vector with the converted times
#' @export
#' @rdname convertTime
convertTime <- function(x, from, to) {
  lifecycle::deprecate_warn("1.9.0", "convertTime()", "convert_time()")
  convert_time(x = x, from = from, to = to)
}

setGeneric("convertTime", function(x, from, to) {
  lifecycle::deprecate_warn("1.9.0", "convertTime()", "convert_time()")
  convert_time(x = x, from = from, to = to)
})

#_______________________________________________________________________________
#----                           plan_setup.R                                ----
#_______________________________________________________________________________

#' Setup default plan for the given simulation or hardware settings.
#' This plan will prioritise the distribution of workers in the following order:
#' 1) Replicates (if 'replicate_parallel' is enabled)
#' 2) Scenarios (if 'scenario_parallel' is enabled)
#' 3) Dataset export / slices (if 'dataset_export' or 'slice_parallel' is enabled)  
#' 
#' `setupPlanDefault()` is deprecated in favor of `setup_plan_default()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams setup_plan_default
#' @return nothing
#' @export
setupPlanDefault <- function(object) {
  lifecycle::deprecate_warn("1.9.0", "setupPlanDefault()", "setup_plan_default()")
  setup_plan_default(object)
}

#' Setup plan as sequential (i.e. no parallelisation).
#' 
#' `setupPlanSequential()` is deprecated in favor of `setup_plan_sequential()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @return nothing
#' @export
setupPlanSequential <- function() {
  lifecycle::deprecate_warn("1.9.0", "setupPlanSequential()", "setup_plan_sequential()")
  setup_plan_sequential()
}

