#' 
#' Simulation progress class.
#' 
#' @param replicates total number of replicates to simulate
#' @param scenarios total number of scenarios to simulate
#' @param iterations total number of iterations to simulate
#' @param slices total number of slices to simulate
#' @param replicate current replicate number being simulated
#' @param scenario current scenario number being simulated
#' @param iteration current iteration number being simulated
#' @param slice current slice number being simulated
#' @param progressor progressr progressor
#' @param hardware hardware settings
#' @export
setClass(
  "simulation_progress",
  representation(
    replicates="integer", # Known upfront
    scenarios="integer",  # Known upfront
    iterations="integer", # May vary based on the dataset (see getEventIterations())
    slices="integer",     # May vary based on number of subjects in dataset (which can change in scenarios)
    replicate="integer",
    scenario="integer",
    iteration="integer",
    slice="integer",
    progressor="ANY",
    hardware="hardware_settings"
  ),
  validity=function(object) {
    return(c(expectOne(object, "replicates"),
             expectOne(object, "scenarios"),
             expectOne(object, "iterations"),
             expectOne(object, "slices"),
             expectOne(object, "replicate"),
             expectOne(object, "iteration"),
             expectOne(object, "scenario"),
             expectOne(object, "slice")))
  },
  prototype=prototype(slices=0L, replicate=0L, scenario=0L, iteration=0L, slice=0L)
)

#' 
#' Create a simulation progress object.
#' 
#' @param replicates total number of replicates to simulate
#' @param scenarios total number of scenarios to simulate
#' @param progressor progressr progressor
#' @param hardware hardware settings
#' @return a progress bar
#' @importFrom progressr progressor
#' @export
SimulationProgress <- function(replicates=1, scenarios=1, progressor, hardware) {
  return(new("simulation_progress",
             replicates=as.integer(replicates),
             scenarios=as.integer(scenarios),
             iterations=1L,
             progressor=progressor,
             hardware=hardware))
}

#' Compute incremental progress.
#' 
#' @param object simulation progress object
#' @return incremental progress, in percentage
#' @keywords internal
computeIncrementalProgress <- function(object) {
  incrementalWorkPercentage <- 1/(object@replicates*object@scenarios*object@iterations*object@slices)
  return(incrementalWorkPercentage*100)
}

tick <- function(object) {
  increment <- object %>% computeIncrementalProgress()
  if (object@replicates > 1) {
    customMessage <- paste0("Simulating replicate ", object@replicate, "/", object@replicates)
  } else if (object@scenarios > 1) {
    customMessage <- paste0("Simulating scenario ", object@scenario, "/", object@scenarios)
  } else if (object@iterations > 1) {
    customMessage <- paste0("Simulating iteration ", object@iteration, "/", object@iterations)
  } else {
    customMessage <- paste0("Simulating slice ", object@slice, "/", object@slices)
  }
  if (object@hardware@cpu > 1) {
    cpus <- paste0("cpu=", object@hardware@cpu)
    if (object@replicates > 1) {
      customMessage <- paste0("Simulating replicates in parallel (", cpus, ")")
    } else {
      customMessage <- paste0("Running simulation in parallel (", cpus, ")")
    }
  }
  object@progressor(message=customMessage, amount=increment)
  return(object)
}

updateReplicate <- function(object, index) {
  object@replicate <- as.integer(index)
  object@scenario <- 0L
  object@iteration <- 0L
  object@slice <- 0L
  return(object)
}

updateScenario <- function(object, index) {
  object@scenario <- as.integer(index)
  object@iteration <- 0L
  object@slice <- 0L
  return(object)
}

updateIteration <- function(object, index) {
  object@iteration <- as.integer(index)
  object@slice <- 0L
  return(object)
}

updateSlice <- function(object, index) {
  object@slice <- as.integer(index)
  return(object)
}

#' Suggested Campsis handler for showing the progress bar.
#' 
#' @return a progressr handler list
#' @export
campsis_handler <- function() {
  return(list(
    progressr::handler_progress(
      format=" :message [:bar] :percent eta: :eta",
      width=100
    )
  ))
}
