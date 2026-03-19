
#_______________________________________________________________________________
#----                         scenario class                                ----
#_______________________________________________________________________________

checkScenario <- function(object) {
  checkName <- expectOne(object, "name")
  checkModel <- expectAppropriateModelArg(object@model)
  checkDataset <- expectAppropriateDatasetArg(object@dataset)
  return(c(checkName, checkModel, checkDataset))
}

#' 
#' Scenario class.
#' 
#' @slot name scenario name, single character string
#' @slot model either a Campsis model, a function or lambda-style formula
#' @slot dataset either a Campsis dataset, a function or lambda-style formula
#' @slot actions list of actions to apply
#' @export
setClass(
  "scenario",
  representation(
    name = "character",
    model = "ANY", # To deprecate
    dataset = "ANY", # To deprecate
    actions = "scenario_actions"
  ),
  contains="pmx_element",
  validity=checkScenario
)

#' 
#' Create an scenario.
#' 
#' @param name scenario name, single character string
#' @param model either a Campsis model, a function or lambda-style formula
#' @param dataset either a Campsis dataset, a function or lambda-style formula
#' @return a new scenario
#' @export
Scenario <- function(name=NULL, model=NULL, dataset=NULL) {
  if (is.null(name)) {
    name <- as.character(NA)
  }
  if (is.null(model)) {
    model <- ~.x
  } else {
    checkModel <- expectAppropriateModelArg(model)
    assertthat::assert_that(length(checkModel)==0, msg=checkModel)
  }
  if (is.null(dataset)) {
    dataset <- ~.x
  } else {
    checkDataset <- expectAppropriateDatasetArg(dataset)
    assertthat::assert_that(length(checkDataset)==0, msg=checkDataset)
  }
  return(new("scenario", name=name, model=model, dataset=dataset))
}

expectAppropriateModelArg <- function(model) {
  if (is(model, "campsis_model") || is.function(model) || rlang::is_formula(model)) {
    return(character(0))
  } else {
    return("model must be a CAMPSIS model, a function or a purrr-style lambda formula")
  }
}

expectAppropriateDatasetArg <- function(dataset) {
  if (is(dataset, "dataset") || is.function(dataset) || rlang::is_formula(dataset)) {
    return(character(0))
  } else {
    return("dataset must be a CAMPSIS dataset, a function or a purrr-style lambda formula")
  }
}

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________


setMethod("add", signature = c("scenario", "scenario_action"), definition = function(object, x) {
  object@actions <- object@actions %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                           getName                                     ----
#_______________________________________________________________________________

setMethod("getName", signature = c("scenario"), definition = function(x) {
  return(paste0("SCENARIO (", x@name, ")"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("scenario", "json_element"), definition=function(object, json) {
  jsonScenario <- json@data
  scenario <- Scenario(name=jsonScenario$name)
  scenario@actions <- loadFromJSON(new("scenario_actions"), JSONElement(jsonScenario$actions)) 
  return(scenario)
})

#_______________________________________________________________________________
#----                        applyScenario                                  ----
#_______________________________________________________________________________

#' 
#' Apply scenario to the given model or dataset.
#' 
#' @param x the given model or dataset
#' @param scenario the scenario to be applied
#' @return an updated model or dataset
#' @importFrom assertthat assert_that
#' @importFrom rlang as_function is_formula
#' @export
#' @keywords internal
applyScenario <- function(x, scenario) {
  assertthat::assert_that(is(scenario, "scenario"),
                          msg="scenario must be a scenario")
  if (is(x, "campsis_model")) {
    x_ <- scenario@model
  } else if (is(x, "dataset") || is.data.frame(x)) {
    x_ <- scenario@dataset
  } else {
    stop("x must be either a CAMPSIS model or dataset")
  }

  if (is.function(x_)) {
    retValue <- x_(x) 
  } else if (rlang::is_formula(x_)) {
    x_ <- rlang::as_function(x_)
    retValue <- x_(x)
  } else {
    retValue <- x_
  }
  
  for (action in scenario@actions@list) {
    retValue <- retValue %>%
      applyAction(action=action)
  }
  
  return(retValue)
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("scenario"), definition=function(object) {
  cat(sprintf("Scenario '%s'", object@name), "\n", sep="")
  for (action in object@actions@list) {
    cat(" - ")
    show(action)
  }
})
