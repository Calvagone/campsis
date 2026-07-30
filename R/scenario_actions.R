#_______________________________________________________________________________
#----                       scenario_actions class                          ----
#_______________________________________________________________________________

#'
#' Scenario actions class.
#'
#' @export
setClass(
  "scenario_actions",
  representation(),
  contains = "pmx_list",
  prototype = prototype(type = "scenario_action")
)

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("scenario_actions", "json_element"), definition = function(object, json) {
  for (jsonAction in json@data) {
    if (jsonAction$type == "replace_action") {
      object <- object %>%
        add(load_from_json(ReplaceAction(NA), JSONElement(jsonAction)))
    } else {
      stop("Only replacement actions are supported for now")
    }
  }
  return(object)
})
