#_______________________________________________________________________________
#----                       simulation_settings class                       ----
#_______________________________________________________________________________

#' 
#' Simulation settings class.
#' 
#' @slot default default settings of the simulate method
#' @slot hardware hardware settings object
#' @slot solver solver settings object
#' @slot nocb NOCB settings object
#' @slot declare declare settings (mrgsolve only)
#' @slot progress progress settings
#' @slot replication replication settings
#' @slot internal internal settings
#' @export
setClass(
  "simulation_settings",
  representation(
    default="default_settings",
    hardware="hardware_settings",
    solver="solver_settings",
    nocb="nocb_settings",
    declare="declare_settings",
    progress="progress_settings",
    replication="replication_settings",
    internal="internal_settings"
  ),
  prototype=prototype(hardware=Hardware(), solver=Solver(), nocb=NOCB(), declare=Declare(),
                      progress=Progress(), replication=AutoReplicationSettings())
)

#'
#' Create advanced simulation settings.
#'
#' @param ... any user-required settings: see ?Hardware, ?Solver, ?NOCB, ?Declare, ?Progress or ?AutoReplicationSettings
#' @param json path to JSON settings file or JSON content in string form
#' @return advanced simulation settings
#' @importFrom purrr detect
#' @export
Settings <- function(..., json=NULL) {
  if (!is.null(json)) {
    schema <- system.file("extdata", "campsis_settings.schema.json", package="campsis")
    return(loadFromJSON(new("simulation_settings"), openJSON(json=json, schema=schema)))
  }
  args <- list(...)
  
  # Check if hardware settings are specified
  default <- args %>% purrr::detect(~(is(.x, "default_settings")))
  if (is.null(default)) {
    default <- new("default_settings")
  }
  
  # Check if hardware settings are specified
  hardware <- args %>% purrr::detect(~(is(.x, "hardware_settings")))
  if (is.null(hardware)) {
    hardware <- Hardware()
  }
  
  # Check if solver settings are specified
  solver <- args %>% purrr::detect(~(is(.x, "solver_settings")))
  if (is.null(solver)) {
    solver <- Solver()
  }
  
  # Check if NOCB settings are specified
  nocb <- args %>% purrr::detect(~(is(.x, "nocb_settings")))
  if (is.null(nocb)) {
    nocb <- NOCB()
  }
  
  # Check if declare settings are specified
  declare <- args %>% purrr::detect(~(is(.x, "declare_settings")))
  if (is.null(declare)) {
    declare <- Declare()
  }
  
  # Check if progress settings are specified
  progress <- args %>% purrr::detect(~(is(.x, "progress_settings")))
  if (is.null(progress)) {
    progress <- Progress()
  }
  
  # Check if replication settings are specified
  replication <- args %>% purrr::detect(~(is(.x, "replication_settings")))
  if (is.null(replication)) {
    replication <- AutoReplicationSettings()
  }
  
  # Check no other argument remains
  others <- args %>%  purrr::discard(~(is(.x, "default_settings") ||
                                       is(.x, "hardware_settings") ||
                                       is(.x, "solver_settings") ||
                                       is(.x, "nocb_settings") ||
                                       is(.x, "declare_settings") ||
                                       is(.x, "progress_settings") ||
                                       is(.x, "replication_settings")  
                                       ))
  assertthat::assert_that(length(others) == 0,
                          msg="Unknown argument detected. Accepted settings: see ?DefaultSettings, ?Hardware, ?Solver, ?NOCB, ?Declare, ?Progress, ?AutoReplicationSettings")
  
  return(new("simulation_settings", default=default, hardware=hardware, solver=solver, nocb=nocb,
             declare=declare, progress=progress, replication=replication))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

setMethod("add", signature = c("simulation_settings", "default_settings"), definition = function(object, x) {
  object@default <- x
  return(object)
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("simulation_settings", "json_element"), definition=function(object, json) {
  object <- jsonToCampsisSettings(object, json)
  return(object)
})

setMethod("loadFromJSON", signature=c("simulation_settings", "character"), definition=function(object, json) {
  schema <- system.file("extdata", "campsis_settings.schema.json", package="campsis")
  return(loadFromJSON(object=object, json=openJSON(json=json, schema=schema)))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("simulation_settings"), definition=function(object) {
  cat("Simulation settings:\n")
  show(object@default)
  show(object@hardware)
  show(object@solver)
  show(object@nocb)
  show(object@declare)
  show(object@progress)
  show(object@replication)
})
