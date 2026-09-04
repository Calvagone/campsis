#_______________________________________________________________________________
#----                         scenario class                                ----
#_______________________________________________________________________________

check_scenario <- function(object) {
  checkName <- expect_one(object, "name")
  checkModel <- expect_appropriate_model_arg(object@model)
  checkDataset <- expect_appropriate_dataset_arg(object@dataset)
  return(c(checkName, checkModel, checkDataset))
}

#'
#' Scenario class.
#'
#' @slot name scenario name, single character string
#' @slot model either a Campsis model, a function or lambda-style formula
#' @slot dataset either a Campsis dataset, a function or lambda-style formula
#' @slot actions list of actions to apply
#' @slot enabled is the scenario enabled in the simulation, logical value
#' @export
setClass(
  "scenario",
  representation(
    name = "character",
    model = "ANY", # To deprecate
    dataset = "ANY", # To deprecate
    actions = "scenario_actions",
    enabled = "logical"
  ),
  contains = "pmx_element",
  prototype = prototype(enabled = TRUE),
  validity = check_scenario
)

#'
#' Create an scenario.
#'
#' @param name scenario name, single character string
#' @param model either a Campsis model, a function or lambda-style formula
#' @param dataset either a Campsis dataset, a function or lambda-style formula
#' @return a new scenario
#' @export
Scenario <- function(name = NULL, model = NULL, dataset = NULL) {
  if (is.null(name)) {
    name <- as.character(NA)
  }
  if (is.null(model)) {
    model <- ~.x
  } else {
    checkModel <- expect_appropriate_model_arg(model)
    assertthat::assert_that(length(checkModel) == 0, msg = checkModel)
  }
  if (is.null(dataset)) {
    dataset <- ~.x
  } else {
    checkDataset <- expect_appropriate_dataset_arg(dataset)
    assertthat::assert_that(length(checkDataset) == 0, msg = checkDataset)
  }
  return(new("scenario", name = name, model = model, dataset = dataset))
}

expect_appropriate_model_arg <- function(model) {
  if (is(model, "campsis_model") || is.function(model) || rlang::is_formula(model)) {
    return(character(0))
  } else {
    return("model must be a Campsis model, a function or a purrr-style lambda formula")
  }
}

expect_appropriate_dataset_arg <- function(dataset) {
  if (is(dataset, "dataset") || is.function(dataset) || rlang::is_formula(dataset)) {
    return(character(0))
  } else {
    return("dataset must be a Campsis dataset, a function or a purrr-style lambda formula")
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
#----                              disable                                  ----
#_______________________________________________________________________________

setMethod("disable", signature = c("scenario", "logical"), definition = function(object, x, ...) {
  if (length(x) == 1) {
    object@enabled <- !x
  } else {
    stop("x should be TRUE or FALSE")
  }
  return(object)
})

#_______________________________________________________________________________
#----                           get_name                                     ----
#_______________________________________________________________________________

setMethod("get_name", signature = c("scenario"), definition = function(x) {
  return(paste0("SCENARIO (", x@name, ")"))
})

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("scenario", "json_element"), definition = function(object, json) {
  json_actions <- json@data$actions
  json@data$actions <- NULL
  scenario <- campsismod::map_json_properties_to_s4_slots(object, json)
  scenario@actions <- load_from_json(new("scenario_actions"), JSONElement(json_actions))
  return(scenario)
})

#_______________________________________________________________________________
#----                        apply_scenario                                  ----
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
apply_scenario <- function(x, scenario) {
  assertthat::assert_that(is(scenario, "scenario"), msg = "scenario must be a scenario")
  if (is(x, "campsis_model")) {
    x_ <- scenario@model
  } else if (is(x, "dataset") || is.data.frame(x)) {
    x_ <- scenario@dataset
  } else {
    stop("x must be either a Campsis model or dataset")
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
      apply_action(action = action)
  }

  return(retValue)
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("scenario"), definition = function(object) {
  disabled_str = ""
  if (!object@enabled) {
    disabled_str = " (DISABLED)"
  }
  cat(sprintf("Scenario '%s'%s", object@name, disabled_str), "\n", sep = "")
  for (action in object@actions@list) {
    cat(" - ")
    show(action)
  }
})
