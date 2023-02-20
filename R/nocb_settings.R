#_______________________________________________________________________________
#----                          nocb_settings class                          ----
#_______________________________________________________________________________

#' 
#' NOCB settings class.
#' 
#' @slot enable enable/disable next-observation carried backward mode (NOCB), default value is TRUE for mrgsolve, FALSE for RxODE
#' @slot variables variable names subject to NOCB behavior (see vignette for more info)
#' @export
setClass(
  "nocb_settings",
  representation(
    enable="logical",
    variables="character"
  ),
  prototype=prototype(enable=as.logical(NA), variables=character(0))
)

#'
#' Create NOCB settings.
#'
#' @param enable enable/disable next-observation carried backward mode (NOCB), default value is TRUE for mrgsolve, FALSE for RxODE
#' @param variables variable names subject to NOCB behavior (see vignette for more info)
#'
#' @return NOCB settings
#' @export
NOCB <- function(enable=NULL, variables=character(0)) {
  if (is.null(enable)) {
    enable <- as.logical(NA)
  }
  return(new("nocb_settings", enable=enable, variables=variables))
}
