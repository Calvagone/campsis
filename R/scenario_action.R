#_______________________________________________________________________________
#----                        scenario_action class                          ----
#_______________________________________________________________________________

#'
#' Scenario action class.
#'
#' @export
setClass(
  "scenario_action",
  representation(),
  contains = "pmx_element"
)

setMethod("get_name", signature = c("scenario_action"), definition = function(x) {
  return(as.character(NA))
})

#_______________________________________________________________________________
#----                         replace_action class                          ----
#_______________________________________________________________________________

#'
#' Replace action class.
#'
#' @export
setClass(
  "replace_action",
  representation(
    object = "ANY"
  ),
  contains = "scenario_action"
)

#'
#' Create a replace action.
#'
#' @param object replacement object
#' @return a replace action
#' @export
ReplaceAction <- function(object) {
  return(new("replace_action", object = object))
}

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("replace_action", "json_element"), definition = function(object, json) {
  replacementObject <- json@data$object
  replacementType <- replacementObject$type
  if (replacementType %in% c("theta", "omega", "sigma")) {
    object@object <- campsismod::json_to_parameter(replacementObject)
  } else {
    stop(sprintf("Type 's' is not supported as a replacement object", replacementType))
  }
  return(object)
})

#_______________________________________________________________________________
#----                            apply_action                               ----
#_______________________________________________________________________________

#' @rdname apply_action
setMethod("apply_action", signature = c("campsis_model", "replace_action"), definition = function(object, action) {
  replacementObject <- action@object
  if (is(replacementObject, "parameter")) {
    object <- object %>%
      campsismod::replace(replacementObject)
  }
  return(object)
})

#' @rdname apply_action
setMethod("apply_action", signature = c("dataset", "replace_action"), definition = function(object, action) {
  # Nothing to do yet
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("replace_action"), definition = function(object) {
  cat("Replacement object:\n")
  show(object@object)
  cat("\n")
})
