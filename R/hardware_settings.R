#_______________________________________________________________________________
#----                       hardware_settings class                       ----
#_______________________________________________________________________________

#' 
#' Hardware settings class.
#' 
#' @slot cpu number of CPU's to use, default is 1
#' @slot parallel enable parallel computing, default is FALSE
#' @slot slices number of subjects per simulated slice, default is 5
#' @slot dataset_parallel_export enable parallelisation when exporting dataset into a table, default is FALSE
#' @slot dataset_slice_size dataset slice size when exporting subjects to a table, default is 250. Only applicable if 'dataset_parallel_export' is enabled.
#' @export
setClass(
  "hardware_settings",
  representation(
    cpu="integer",
    parallel="logical",
    slices="integer",
    dataset_parallel_export="logical",
    dataset_slice_size="integer"
  ),
  prototype=prototype(cpu=as.integer(1), parallel=FALSE, slices=as.integer(5),
                      dataset_parallel_export=FALSE, dataset_slice_size=as.integer(250))
)

#'
#' Create hardware settings.
#'
#' @param cpu number of CPU's to use, default is 1
#' @param parallel enable parallel computing, default is FALSE
#' @param slices number of subjects per simulated slice, default is 5
#' @param dataset_parallel_export enable parallelisation when exporting dataset into a table, default is FALSE
#' @param dataset_slice_size dataset slice size when exporting subjects to a table, default is 250. Only applicable if 'dataset_parallel_export' is enabled.
#'
#' @return hardware settings
#' @export
Hardware <- function(cpu=1, parallel=FALSE, slices=5, dataset_parallel_export=FALSE, dataset_slice_size=250) {
  return(new("hardware_settings", cpu=as.integer(cpu), parallel=parallel, slices=as.integer(slices),
             dataset_parallel_export=dataset_parallel_export, dataset_slice_size=as.integer(dataset_slice_size)))
}
