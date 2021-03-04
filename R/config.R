
#_______________________________________________________________________________
#----                          config class                                 ----
#_______________________________________________________________________________

checkConfig <- function(object) {
  return(expectOneForAll(object, c("default_depot_cmt", "default_obs_cmt")))
}

#' 
#' Config class.
#' 
#' @export
setClass(
  "config",
  representation(
    default_depot_cmt = "integer",
    default_obs_cmt = "integer"
  ),
  prototype=prototype(default_depot_cmt=as.integer(1), default_obs_cmt=as.integer(1)),
  validity= checkConfig
)