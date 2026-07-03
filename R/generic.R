
no_default_function_provided_debug <- function(args_list, fun_name) {
  # Get the class of each argument as a string (fixed CHARACTER to character)
  arg_classes <- vapply(args_list, function(x) {
    if (is.null(x)) "NULL" else paste(class(x), collapse = "/")
  }, character(1))
  
  # Format into a readable string: "arg1 (class), arg2 (class), ..."
  formatted_args <- sprintf("%s (%s)", names(arg_classes), arg_classes)
  error_details <- paste(formatted_args, collapse = "\n  ")
  
  stop(paste0(
    "Generic '", fun_name, "' function cannot be called directly.\n",
    "Received arguments:\n  ", error_details
  ), call. = FALSE)
}

#_______________________________________________________________________________
#----                            applyAction                                ----
#_______________________________________________________________________________

#' Apply some action on the given object.
#' 
#' @param object any object
#' @param action action to apply
#' @return updated object
#' @export
#' @rdname applyAction
applyAction <- function(object, action) {
  stop("No default function is provided")
}

setGeneric("applyAction", function(object, action) {
  standardGeneric("applyAction")
})

#_______________________________________________________________________________
#----                            apply_outfun                               ----
#_______________________________________________________________________________

#' Apply output function(s) on the given Campsis results.
#' 
#' @param x Campsis simulation results
#' @param outfun output function(s), an `outfun` or `outfuns` object
#' @param level level at which the output function is applied, e.g. 'replicate'
#' @param ... extra arguments transmitted automatically by Campsis (e.g. `replicate`, `scenario`)
#' @return the updated Campsis results
#' @export
#' @rdname apply_outfun
apply_outfun <- function(x, outfun, level, ...) {
  stop("No default function is provided")
}

setGeneric("apply_outfun", function(x, outfun, level, ...) {
  standardGeneric("apply_outfun")
}, signature = "outfun")

#_______________________________________________________________________________
#----                           getCovariates                               ----
#_______________________________________________________________________________

#' Get all covariates (fixed / time-varying / event covariates).
#' 
#' @param object any object
#' @return all covariates from object
#' @export
#' @rdname getCovariates
getCovariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("getCovariates", function(object) {
  standardGeneric("getCovariates")
})

#_______________________________________________________________________________
#----                         getEventCovariates                            ----
#_______________________________________________________________________________

#' Get all event-related covariates.
#' 
#' @param object any object
#' @return all event-related covariates from object
#' @export
#' @rdname getEventCovariates
getEventCovariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("getEventCovariates", function(object) {
  standardGeneric("getEventCovariates")
})

#_______________________________________________________________________________
#----                         getFixedCovariates                            ----
#_______________________________________________________________________________

#' Get all fixed covariates.
#' 
#' @param object any object
#' @return all fixed covariates from object
#' @export
#' @rdname getFixedCovariates
getFixedCovariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("getFixedCovariates", function(object) {
  standardGeneric("getFixedCovariates")
})

#_______________________________________________________________________________
#----                       getTimeVaryingCovariates                        ----
#_______________________________________________________________________________

#' Get all time-varying covariates.
#' 
#' @param object any object
#' @return all time-varying covariates from object
#' @export
#' @rdname getTimeVaryingCovariates
getTimeVaryingCovariates <- function(object) {
  stop("No default function is provided")
}

setGeneric("getTimeVaryingCovariates", function(object) {
  standardGeneric("getTimeVaryingCovariates")
})

#_______________________________________________________________________________
#----                              getIOVs                                  ----
#_______________________________________________________________________________

#' Get all IOV objects.
#' 
#' @param object any object
#' @return all IOV's from object
#' @export
#' @rdname getIOVs
getIOVs <- function(object) {
  stop("No default function is provided")
}

setGeneric("getIOVs", function(object) {
  standardGeneric("getIOVs")
})

#_______________________________________________________________________________
#----                            getOccasions                               ----
#_______________________________________________________________________________

#' Get all occasions.
#' 
#' @param object any object
#' @return all occasions from object
#' @export
#' @rdname getOccasions
getOccasions <- function(object) {
  stop("No default function is provided")
}

setGeneric("getOccasions", function(object) {
  standardGeneric("getOccasions")
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' Get all distinct times for the specified object.
#' 
#' @param object any object
#' @param ... extra arguments like `doseTimes` in observations or `unwrap` in treatment
#' @return numeric vector with all unique times, sorted
#' @export
#' @rdname getTimes
getTimes <- function(object, ...) {
  stop(sprintf("No default function is provided for 'object': %s", class(object)))
}

setGeneric("getTimes", function(object, ...) {
  standardGeneric("getTimes")
})

#_______________________________________________________________________________
#----                           repeatSchedule                              ----
#_______________________________________________________________________________

#' Repeat schedule.
#' 
#' @param x object to repeat the schedule
#' @param schedule initial times vector
#' @return resulting times vector
#' @export
#' @rdname repeatSchedule
repeatSchedule <- function(x, schedule) {
  stop("No default function is provided")
}

setGeneric("repeatSchedule", function(x, schedule) {
  standardGeneric("repeatSchedule")
})

#_______________________________________________________________________________
#----                             setLabel                                  ----
#_______________________________________________________________________________

#' Set the label.
#' 
#' @param object any object that has a label
#' @param x the new label
#' @return the updated object
#' @export
#' @rdname setLabel
setLabel <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("setLabel", function(object, x) {
  standardGeneric("setLabel")
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
#----                           setSubjects                                 ----
#_______________________________________________________________________________

#' Set the number of subjects.
#' 
#' @param object any object
#' @param x the new number of subjects
#' @return the updated object
#' @export
#' @rdname setSubjects
setSubjects <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("setSubjects", function(object, x) {
  if (is.numeric(x)) {
    x <- as.integer(x)
  }
  standardGeneric("setSubjects")
})

#_______________________________________________________________________________
#----                          unwrapTreatment                              ----
#_______________________________________________________________________________

#' Unwrap treatment.
#' 
#' @param object any object
#' @return updated object
#' @export
#' @rdname unwrapTreatment
unwrapTreatment <- function(object) {
  stop("No default function is provided")
}

setGeneric("unwrapTreatment", function(object) {
  standardGeneric("unwrapTreatment")
})

#_______________________________________________________________________________
#----                            updateAmount                               ----
#_______________________________________________________________________________

#' Update amount.
#' 
#' @param object generic object
#' @param amount new amount
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname updateAmount
updateAmount <- function(object, amount, ref) {
  stop("No default function is provided")
}

setGeneric("updateAmount", function(object, amount, ref) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  standardGeneric("updateAmount")
})

#_______________________________________________________________________________
#----                              updateII                                 ----
#_______________________________________________________________________________

#' Update the inter-dose interval (II).
#' 
#' @param object generic object
#' @param ii new inter-dose interval
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname updateII
updateII <- function(object, ii, ref) {
  stop("No default function is provided")
}

setGeneric("updateII", function(object, ii, ref=NULL) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  standardGeneric("updateII")
})

#_______________________________________________________________________________
#----                             updateADDL                                ----
#_______________________________________________________________________________

#' Update the number of additional doses (ADDL).
#' 
#' @param object generic object
#' @param addl new number of additional doses
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname updateADDL
updateADDL <- function(object, addl, ref) {
  stop("No default function is provided")
}

setGeneric("updateADDL", function(object, addl, ref=NULL) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  addl <- as.integer(addl)
  standardGeneric("updateADDL")
})

#_______________________________________________________________________________
#----                            updateRepeat                               ----
#_______________________________________________________________________________

#' Update the repeat field (argument 'rep' in Bolus and Infusion constructors).
#' 
#' @param object generic object
#' @param rep repeated dosing schedule (definition) object
#' @param ref reference treatment name
#' @return updated object
#' @export
#' @rdname updateRepeat
updateRepeat <- function(object, rep, ref) {
  stop("No default function is provided")
}

setGeneric("updateRepeat", function(object, rep, ref=NULL) {
  if (is.null(ref)) {
    ref <- as.character(NA)
  }
  standardGeneric("updateRepeat")
})

