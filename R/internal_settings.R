
#_______________________________________________________________________________
#----                        internal_settings class                        ----
#_______________________________________________________________________________

#' 
#' Internal settings class (transient object from the simulation settings).
#' 
#' @slot dataset_summary dataset summary
#' @slot progress simulation progress
#' @slot iterations list of event iterations
setClass(
  "internal_settings",
  representation(
    dataset_summary="dataset_summary",
    progress="simulation_progress",
    iterations="list" # list of event iterations
  )
)
