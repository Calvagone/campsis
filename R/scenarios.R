
#_______________________________________________________________________________
#----                         scenarios class                               ----
#_______________________________________________________________________________

#' 
#' Scenarios class.
#' 
#' @export
setClass(
  "scenarios",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="scenario") 
)

#' 
#' Create a list of scenarios.
#' 
#' @param json path to JSON scenarios file or JSON content in string form
#' @return a scenarios object
#' @export
Scenarios <- function(json=NULL) {
  if (is.null(json)) {
    scenarios <- new("scenarios")
  } else {
    schema <- system.file("extdata", "no_sub_schemas", "campsis_scenarios.schema.json", package="campsis") 
    scenarios <- load_from_json(new("scenarios"), open_json(json=json, schema=schema))
  }
  return(scenarios)
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

#' @importFrom methods callNextMethod
setMethod("add", signature = c("scenarios", "scenario"), definition = function(object, x) {
  # Create default name is name was not set
  if (is.na(x@name)) {
    x@name <- paste("Scenario", object %>% length() + 1)
  } 
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("scenarios", "json_element"), definition=function(object, json) {
  for (jsonScenario in json@data) {
    scenario <- load_from_json(object=Scenario(), json=JSONElement(jsonScenario))
    object <- object %>%
      add(scenario)
  }
  return(object)
})

setMethod("load_from_json", signature=c("scenarios", "character"), definition=function(object, json) {
  schema <- system.file("extdata", "no_sub_schemas", "campsis_scenarios.schema.json", package="campsis")
  return(load_from_json(object=object, json=open_json(json=json, schema=schema)))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("scenarios"), definition=function(object) {
  for (scenario in object@list) {
    show(scenario)
    cat("\n")
  }
})
