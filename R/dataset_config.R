
#_______________________________________________________________________________
#----                       dataset_config class                            ----
#_______________________________________________________________________________

checkConfig <- function(object) {
  return(expectOneForAll(object, c("def_depot_cmt", "def_obs_cmt")))
}

#' 
#' Dataset config class.
#' 
#' @slot def_depot_cmt default depot compartment, integer
#' @slot def_obs_cmt default observation compartment, integer
#' @export
setClass(
  "dataset_config",
  representation(
    def_depot_cmt = "integer",
    def_obs_cmt = "integer"
  ),
  prototype=prototype(def_depot_cmt=as.integer(1), def_obs_cmt=as.integer(1)),
  validity= checkConfig
)

#'
#' Create a dataset configuration. This configuration allows CAMPSIS to know which
#' are the default depot and observed compartments.
#'
#' @param defDepotCmt default depot compartment, integer
#' @param defObsCmt default observation compartment, integer
#' @return a dataset configuration
#' @export
DatasetConfig <- function(defDepotCmt=1, defObsCmt=1) {
  return(new("dataset_config", def_depot_cmt=as.integer(defDepotCmt), def_obs_cmt=as.integer(defObsCmt)))
}

