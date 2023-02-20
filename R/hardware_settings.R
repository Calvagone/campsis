#_______________________________________________________________________________
#----                       hardware_settings class                       ----
#_______________________________________________________________________________

#' 
#' Hardware settings class.
#' 
#' @slot cpu number of CPU's to use, default is 6
#' @slot parallel enable parallel computing, logical value
#' @export
setClass(
  "hardware_settings",
  representation(
    cpu="integer",
    parallel="logical"
  ),
  prototype=prototype(cpu=as.integer(6), parallel=FALSE)
)

#'
#' Create hardware settings.
#'
#' @param cpu number of CPU's to use, default is 6
#' @param parallel enable parallel computing, logical value
#'
#' @return hardware settings
#' @export
Hardware <- function(cpu=6, parallel=FALSE) {
  return(new("hardware_settings", cpu=as.integer(cpu), parallel=parallel))
}
