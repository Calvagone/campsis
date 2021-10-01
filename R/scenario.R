
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
#' @slot model either a CAMPSIS model, a function or lambda-style formula
#' @slot dataset either a CAMPSIS dataset, a function or lambda-style formula
#' @export
setClass(
  "scenario",
  representation(
    name = "character",
    model = "ANY",
    dataset = "ANY"
  ),
  contains="pmx_element",
  validity=checkScenario
)

#' 
#' Create an scenario.
#' 
#' @param name scenario name, single character string
#' @param model either a CAMPSIS model, a function or lambda-style formula
#' @param dataset either a CAMPSIS dataset, a function or lambda-style formula
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
  if (is(model, "campsis_model") || is.function(model) || plyr::is.formula(model)) {
    return(character(0))
  } else {
    return("model must be a CAMPSIS model, a function or a lambda formula")
  }
}

expectAppropriateDatasetArg <- function(dataset) {
  if (is(dataset, "dataset") || is.function(dataset) || plyr::is.formula(dataset)) {
    return(character(0))
  } else {
    return("dataset must be a CAMPSIS dataset, a function or a lambda formula")
  }
}

#_______________________________________________________________________________
#----                           getName                                     ----
#_______________________________________________________________________________

setMethod("getName", signature = c("scenario"), definition = function(x) {
  return(paste0("SCENARIO (", x@name, ")"))
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
#' @importFrom plyr is.formula
#' @importFrom rlang as_function
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
    return(x_(x))
  } else if (plyr::is.formula(x_)) {
    x_ <- rlang::as_function(x_)
    return(x_(x))
  } else {
    return(x_)
  }
}