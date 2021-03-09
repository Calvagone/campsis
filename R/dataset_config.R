
#_______________________________________________________________________________
#----                       dataset_config class                            ----
#_______________________________________________________________________________

checkConfig <- function(object) {
  return(expectOneForAll(object, c("def_depot_cmt", "def_obs_cmt")))
}

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
#' Create dataset configuration.
#'
#' @param defDepotCmt default depot compartment, integer
#' @param defObsCmt default observation compartment, integer
#' @return configuration
#' @export
DatasetConfig <- function(defDepotCmt=1, defObsCmt=1) {
  return(new("dataset_config", def_depot_cmt=as.integer(defDepotCmt), def_obs_cmt=as.integer(defObsCmt)))
}

