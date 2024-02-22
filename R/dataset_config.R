
#_______________________________________________________________________________
#----                       dataset_config class                            ----
#_______________________________________________________________________________

#' 
#' Dataset configuration class.
#' 
#' @slot def_depot_cmt default depot compartment, integer
#' @slot def_obs_cmt default observation compartment, integer
#' @slot export_tsld export column TSLD, logical
#' @slot export_tdos export column TDOS, logical
#' @slot time_unit_dataset unit of time in dataset, character ('hour' by default)
#' @slot time_unit_export unit of time in export, character ('hour' by default)
#' @export
setClass(
  "dataset_config",
  representation(
    def_depot_cmt="integer",
    def_obs_cmt="integer",
    export_tsld="logical",
    export_tdos="logical",
    time_unit_dataset="character",
    time_unit_export="character"
  ),
  prototype=prototype(def_depot_cmt=as.integer(1),
                      def_obs_cmt=as.integer(1),
                      export_tsld=FALSE,
                      export_tdos=FALSE,
                      time_unit_dataset="hour",
                      time_unit_export="hour"),
  validity=function(object) {
    return(expectOneForAll(object, c("def_depot_cmt", "def_obs_cmt", "export_tsld",
                                     "export_tdos", "time_unit_dataset", "time_unit_export")))
  }
)

#'
#' Create a dataset configuration. This configuration allows CAMPSIS to know which
#' are the default depot and observed compartments.
#'
#' @param defDepotCmt default depot compartment, integer
#' @param defObsCmt default observation compartment, integer
#' @param exportTSLD export column TSLD (time since last dose), logical
#' @param exportTDOS export column TDOS (time of last dose), logical
#' @param timeUnitDataset unit of time in dataset, character ('hour' by default)
#' @param timeUnitExport unit of time in export, character ('hour' by default)
#' @return a dataset configuration
#' @export
DatasetConfig <- function(defDepotCmt=1, defObsCmt=1, exportTSLD=FALSE, exportTDOS=FALSE,
                          timeUnitDataset="hour", timeUnitExport="hour") {
  return(new("dataset_config", def_depot_cmt=as.integer(defDepotCmt),
             def_obs_cmt=as.integer(defObsCmt), export_tsld=exportTSLD, export_tdos=exportTDOS,
             time_unit_dataset=timeUnitDataset, time_unit_export=timeUnitExport))
}

