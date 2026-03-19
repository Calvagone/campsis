#_______________________________________________________________________________
#----                        scenario_action class                          ----
#_______________________________________________________________________________

#' 
#' Scenario action class.
#' 
#' @export
setClass(
  "scenario_action",
  representation(
  ),
  contains="pmx_element"
)

setMethod("getName", signature = c("scenario_action"), definition = function(x) {
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
  contains="scenario_action"
)

#' 
#' Create a replace action.
#' 
#' @param object replacement object
#' @return a replace action 
#' @export
ReplaceAction <- function(object) {
  return(new("replace_action", object=object))
}

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("replace_action", "json_element"), definition=function(object, json) {
  replacementObject <- json@data$object
  replacementType <- replacementObject$type
  if (replacementType %in% c("theta", "omega", "sigma")) {
    object@object <- campsismod::jsonToParameter(replacementObject)
  } else {
    stop(sprintf("Type 's' is not supported as a replacement object", replacementType))
  }
  return(object)
})

#_______________________________________________________________________________
#----                            applyAction                                ----
#_______________________________________________________________________________

#' @rdname applyAction
setMethod("applyAction", signature=c("campsis_model", "replace_action"), definition=function(object, action) {
  replacementObject <- action@object
  if (is(replacementObject, "parameter")) {
    object <- object %>%
      campsismod::replace(replacementObject)
  }
  return(object)
})

#' @rdname applyAction
setMethod("applyAction", signature=c("dataset", "replace_action"), definition=function(object, action) {
  # Nothing to do yet
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("replace_action"), definition=function(object) {
  cat("Replacement object:\n")
  show(object@object)
  cat("\n")
})
