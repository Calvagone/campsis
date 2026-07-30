no_default_function_provided_debug <- function(args_list, fun_name) {
  # Get the class of each argument as a string (fixed CHARACTER to character)
  arg_classes <- vapply(
    args_list,
    function(x) {
      if (is.null(x)) "NULL" else paste(class(x), collapse = "/")
    },
    character(1)
  )

  # Format into a readable string: "arg1 (class), arg2 (class), ..."
  formatted_args <- sprintf("%s (%s)", names(arg_classes), arg_classes)
  error_details <- paste(formatted_args, collapse = "\n  ")

  stop(
    paste0(
      "Generic '",
      fun_name,
      "' function cannot be called directly.\n",
      "Received arguments:\n  ",
      error_details
    ),
    call. = FALSE
  )
}

#_______________________________________________________________________________
#----                            apply_action                               ----
#_______________________________________________________________________________

#' Apply some action on the given object.
#'
#' @param object any object
#' @param action action to apply
#' @return updated object
#' @export
#' @rdname apply_action
apply_action <- function(object, action) {
  stop("No default function is provided")
}

setGeneric("apply_action", function(object, action) {
  standardGeneric("apply_action")
})

#_______________________________________________________________________________
#----                            apply_outfun                               ----
#_______________________________________________________________________________

#' Apply output function(s) on the given Campsis results.
#'
#' @param x Campsis simulation results
#' @param outfun output function(s), an `outfun` or `outfuns` object
#' @param level level at which the output function is applied, 'replicate' by default
#' @param ... extra arguments transmitted automatically by Campsis (e.g. `replicate` for the replicate number)
#' @return the updated Campsis results
#' @export
#' @rdname apply_outfun
apply_outfun <- function(x, outfun, level, ...) {
  stop("No default function is provided")
}

setGeneric(
  "apply_outfun",
  function(x, outfun, level = NULL, ...) {
    if (is.null(level)) {
      level <- "replicate"
    }
    standardGeneric("apply_outfun")
  },
  signature = "outfun"
)

#_______________________________________________________________________________
#----                           get_covariates                              ----
#_______________________________________________________________________________

#' Get all covariates (fixed / time-varying / event covariates).
#'
#' @param object any object
#' @return all covariates from object
#' @export
#' @rdname get_covariates
get_covariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_covariates", function(object) {
  standardGeneric("get_covariates")
})

#_______________________________________________________________________________
#----                        get_event_covariates                            ----
#_______________________________________________________________________________

#' Get all event-related covariates.
#'
#' @param object any object
#' @return all event-related covariates from object
#' @export
#' @rdname get_event_covariates
get_event_covariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_event_covariates", function(object) {
  standardGeneric("get_event_covariates")
})

#_______________________________________________________________________________
#----                        get_fixed_covariates                           ----
#_______________________________________________________________________________

#' Get all fixed covariates.
#'
#' @param object any object
#' @return all fixed covariates from object
#' @export
#' @rdname get_fixed_covariates
get_fixed_covariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_fixed_covariates", function(object) {
  standardGeneric("get_fixed_covariates")
})

#_______________________________________________________________________________
#----                     get_time_varying_covariates                       ----
#_______________________________________________________________________________

#' Get all time-varying covariates.
#'
#' @param object any object
#' @return all time-varying covariates from object
#' @export
#' @rdname get_time_varying_covariates
get_time_varying_covariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_time_varying_covariates", function(object) {
  standardGeneric("get_time_varying_covariates")
})

#_______________________________________________________________________________
#----                             get_iovs                                  ----
#_______________________________________________________________________________

#' Get all IOV objects.
#'
#' @param object any object
#' @return all IOV's from object
#' @export
#' @rdname get_iovs
get_iovs <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_iovs", function(object) {
  standardGeneric("get_iovs")
})

#_______________________________________________________________________________
#----                            get_occasions                              ----
#_______________________________________________________________________________

#' Get all occasions.
#'
#' @param object any object
#' @return all occasions from object
#' @export
#' @rdname get_occasions
get_occasions <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_occasions", function(object) {
  standardGeneric("get_occasions")
})

#_______________________________________________________________________________
#----                             get_times                                 ----
#_______________________________________________________________________________

#' Get all distinct times for the specified object.
#'
#' @param object any object
#' @param ... extra arguments like `doseTimes` in observations or `unwrap` in treatment
#' @return numeric vector with all unique times, sorted
#' @export
#' @rdname get_times
get_times <- function(object, ...) {
  stop(sprintf("No default function is provided for 'object': %s", class(object)))
}

setGeneric("get_times", function(object, ...) {
  standardGeneric("get_times")
})

#_______________________________________________________________________________
#----                           repeat_schedule                             ----
#_______________________________________________________________________________

#' Repeat schedule.
#'
#' @param x object to repeat the schedule
#' @param schedule initial times vector
#' @return resulting times vector
#' @export
#' @rdname repeat_schedule
repeat_schedule <- function(x, schedule) {
  stop("No default function is provided")
}

setGeneric("repeat_schedule", function(x, schedule) {
  standardGeneric("repeat_schedule")
})

#_______________________________________________________________________________
#----                             set_label                                  ----
#_______________________________________________________________________________

#' Set the label.
#'
#' @param object any object that has a label
#' @param x the new label
#' @return the updated object
#' @export
#' @rdname set_label
set_label <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("set_label", function(object, x) {
  standardGeneric("set_label")
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' Sample generic object.
#'
#' @param object generic object
#' @param n number of samples required
#' @param ... extra arguments
#' @return sampling result
#' @export
#' @rdname sample
sample <- function(object, n, ...) {
  stop("No default function is provided")
}

setGeneric("sample", function(object, n, ...) {
  standardGeneric("sample")
})

#_______________________________________________________________________________
#----                           set_subjects                                ----
#_______________________________________________________________________________

#' Set the number of subjects.
#'
#' @param object any object
#' @param x the new number of subjects
#' @return the updated object
#' @export
#' @rdname set_subjects
set_subjects <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("set_subjects", function(object, x) {
  if (is.numeric(x)) {
    x <- as.integer(x)
  }
  standardGeneric("set_subjects")
})

#_______________________________________________________________________________
#----                          unwrap_treatment                             ----
#_______________________________________________________________________________

#' Unwrap treatment.
#'
#' @param object any object
#' @return updated object
#' @export
#' @rdname unwrap_treatment
unwrap_treatment <- function(object) {
  stop("No default function is provided")
}

setGeneric("unwrap_treatment", function(object) {
  standardGeneric("unwrap_treatment")
})

#_______________________________________________________________________________
#----                            update_amount                              ----
#_______________________________________________________________________________

#' Update amount.
#'
#' @param object generic object
#' @param amount new amount
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname update_amount
update_amount <- function(object, amount, ref) {
  stop("No default function is provided")
}

setGeneric("update_amount", function(object, amount, ref) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  standardGeneric("update_amount")
})

#_______________________________________________________________________________
#----                              update_ii                                ----
#_______________________________________________________________________________

#' Update the inter-dose interval (II).
#'
#' @param object generic object
#' @param ii new inter-dose interval
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname update_ii
update_ii <- function(object, ii, ref) {
  stop("No default function is provided")
}

setGeneric("update_ii", function(object, ii, ref = NULL) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  standardGeneric("update_ii")
})

#_______________________________________________________________________________
#----                             update_addl                               ----
#_______________________________________________________________________________

#' Update the number of additional doses (ADDL).
#'
#' @param object generic object
#' @param addl new number of additional doses
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname update_addl
update_addl <- function(object, addl, ref) {
  stop("No default function is provided")
}

setGeneric("update_addl", function(object, addl, ref = NULL) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  addl <- as.integer(addl)
  standardGeneric("update_addl")
})

#_______________________________________________________________________________
#----                            update_repeat                              ----
#_______________________________________________________________________________

#' Update the repeat field (argument 'rep' in Bolus and Infusion constructors).
#'
#' @param object generic object
#' @param rep repeated dosing schedule (definition) object
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname update_repeat
update_repeat <- function(object, rep, ref) {
  stop("No default function is provided")
}

setGeneric("update_repeat", function(object, rep, ref = NULL) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  standardGeneric("update_repeat")
})
