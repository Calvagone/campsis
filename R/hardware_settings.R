#_______________________________________________________________________________
#----                       hardware_settings class                       ----
#_______________________________________________________________________________

#' 
#' Hardware settings class.
#' 
#' @slot cpu number of CPU's to use, default is 6
#' @slot parallel enable parallel computing, logical value
#' @slot slices number of subjects per simulated slice, default is 6
#' @export
setClass(
  "hardware_settings",
  representation(
    cpu="integer",
    parallel="logical",
    slices="integer"
  ),
  prototype=prototype(cpu=as.integer(6), parallel=FALSE, slices=as.integer(6))
)

#'
#' Create hardware settings.
#'
#' @param cpu number of CPU's to use, default is 6
#' @param parallel enable parallel computing, logical value
#' @param slices number of subjects per simulated slice, default is 6
#'
#' @return hardware settings
#' @export
Hardware <- function(cpu=6, parallel=FALSE, slices=6) {
  return(new("hardware_settings", cpu=as.integer(cpu), parallel=parallel, slices=as.integer(slices)))
}
