#_______________________________________________________________________________
#----                       hardware_settings class                       ----
#_______________________________________________________________________________

#' 
#' Hardware settings class.
#' 
#' @slot cpu number of CPU's to use, default is 1
#' @slot replicate_parallel  enable parallel computing for replicates, default is FALSE
#' @slot scenario_parallel  enable parallel computing for scenarios, default is FALSE
#' @slot slice_parallel enable parallel computing for slices, default is FALSE
#' @slot slice_size number of subjects per simulated slice, default is NULL (auto-configured by Campsis depending on the specified engine)
#' @slot dataset_parallel enable parallelisation when exporting dataset into a table, default is FALSE
#' @slot dataset_slice_size dataset slice size when exporting subjects to a table, default is 500. Only applicable if 'dataset_parallel' is enabled.
#' @export
setClass(
  "hardware_settings",
  representation(
    cpu="integer",
    replicate_parallel="logical",
    scenario_parallel="logical",
    slice_parallel="logical",
    slice_size="integer",
    dataset_parallel="logical",
    dataset_slice_size="integer"
  ),
  prototype=prototype(cpu=as.integer(1), replicate_parallel=FALSE, scenario_parallel=FALSE,
                      slice_parallel=FALSE, slice_size=as.integer(NA),
                      dataset_parallel=FALSE, dataset_slice_size=as.integer(500))
)

#'
#' Create hardware settings.
#'
#' @param cpu number of CPU's to use, default is 1
#' @param replicate_parallel  enable parallel computing for replicates, default is FALSE
#' @param slice_parallel enable parallel computing for slices, default is FALSE
#' @param slice_size number of subjects per simulated slice, default is NULL (auto-configured by Campsis depending on the specified engine)
#' @param dataset_parallel enable parallelisation when exporting dataset into a table, default is FALSE
#' @param dataset_slice_size dataset slice size when exporting subjects to a table, default is 500. Only applicable if 'dataset_parallel' is enabled.
#'
#' @return hardware settings
#' @export
Hardware <- function(cpu=1, replicate_parallel=FALSE, scenario_parallel=FALSE,
                     slice_parallel=FALSE, slice_size=NULL, dataset_parallel=FALSE, dataset_slice_size=500) {
  if (is.null(slice_size)) {
    slice_size <- NA
  }
  return(new("hardware_settings", cpu=as.integer(cpu),
             replicate_parallel=replicate_parallel, scenario_parallel=scenario_parallel, 
             slice_parallel=slice_parallel, slice_size=as.integer(slice_size),
             dataset_parallel=dataset_parallel, dataset_slice_size=as.integer(dataset_slice_size)))
}
