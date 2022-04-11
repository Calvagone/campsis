#' 
#' Simulation progress class.
#' 
#' @param replicates total number of replicates to simulate
#' @param scenarios total number of scenarios to simulate
#' @param slices total number of slices to simulate
#' @param replicate current replicate number being simulated
#' @param scenario current scenario number being simulated
#' @param slice current slice number being simulated
#' @export
setClass(
  "simulation_progress",
  representation(
    replicates="integer", # Known upfront
    scenarios="integer",  # Known upfront
    slices="integer",     # May vary based on number of subjects in dataset (which can change in scenarios)
    replicate="integer",
    scenario="integer",
    slice="integer",
    pb="ANY"
  ),
  validity=function(object) {
    return(c(expectOne(object, "replicates"),
             expectOne(object, "scenarios"),
             expectOne(object, "slices"),
             expectOne(object, "replicate"),
             expectOne(object, "scenario"),
             expectOne(object, "slice")))
  },
  prototype=prototype(slices=0L, replicate=0L, scenario=0L, slice=0L)
)

#' 
#' Create a simulation progress object.
#' 
#' @param replicates total number of replicates to simulate
#' @param scenarios total number of scenarios to simulate
#' @return a progress bar
#' @export
SimulationProgress <- function(replicates=1, scenarios=1) {
  return(new("simulation_progress",
             replicates=as.integer(replicates),
             scenarios=as.integer(scenarios),
             pb=progress::progress_bar$new(format=" :custom_field [:bar] :percent eta: :eta",
                                           total=100, force=TRUE)))
}

#' Compute incremental progress.
#' 
#' @param object simulation progress object
#' @return a 
#' @export
computeIncrementalProgress <- function(object) {
  if (!is(object, "simulation_progress")) {
    stop("Not a simulation progress object")
  }

  total <- object@replicates * object@scenarios
  # current <- (object@iteration - 1) +
  #   (object@scenario - 1) * object@iterations +
  #   (object@replicate - 1) * object@scenarios * object@iterations
  # base_unit <- current/total
  sub_unit <- 1/total/object@slices
    
  return(sub_unit*100)
}

tick <- function(object) {
  if (!is(object, "simulation_progress")) {
    stop("Not a simulation progress object")
  }
  increment <- object %>% computeIncrementalProgress()
  # cat(increment)
  # cat("\n")
  
  if (object@replicates > 1) {
    customMessage <- paste0("Simulating replicate ", object@replicate, "/", object@replicates)
  } else if (object@scenarios > 1) {
    customMessage <- paste0("Simulating scenario ", object@scenario, "/", object@scenarios)
  } else {
    customMessage <- paste0("Simulating slice ", object@slice, "/", object@slices)
  }
  object@pb$tick(increment, tokens=list(custom_field=customMessage))
  return(object)
}

updateReplicate <- function(object, replicate) {
  object@replicate <- as.integer(replicate)
  object@scenario <- 0L
  object@slice <- 0L
  return(object)
}

updateScenario <- function(object, scenario) {
  object@scenario <- as.integer(scenario)
  object@slice <- 0L
  return(object)
}

incrementSlice <- function(object) {
  object@slice <- as.integer(object@slice + 1)
  return(object)
}

