#_______________________________________________________________________________
#----                          solver_settings class                        ----
#_______________________________________________________________________________

#' 
#' Solver settings class.
#' See ?mrgsolve::update.
#' See ?rxode2::rxSolve.
#' 
#' @slot atol absolute solver tolerance
#' @slot rtol relative solver tolerance
#' @slot hmax limit how big a solver step can be
#' @slot maxsteps max steps between 2 integration times (e.g. when observations records are far apart)
#' @slot method solver method
#' @export
setClass(
  "solver_settings",
  representation(
    atol="numeric",
    rtol="numeric",
    hmax="numeric",
    maxsteps="integer",
    method="character"
  ),
  prototype=prototype(atol=1e-08, rtol=1e-08, hmax=as.numeric(NA), maxsteps=70000L, method="liblsoda")
)

#'
#' Create solver settings.
#'
#' @slot atol absolute solver tolerance
#' @slot rtol relative solver tolerance
#' @slot hmax limit how big a solver step can be
#' @slot maxsteps max steps between 2 integration times (e.g. when observations records are far apart)
#' @slot method solver method
#'
#' @return solver settings
#' @export
Solver <- function(atol=1e-08, rtol=1e-08, hmax=NA, maxsteps=70000L, method="liblsoda") {
  return(new("solver_settings", atol=atol, rtol=rtol, hmax=as.numeric(hmax), maxsteps=as.integer(maxsteps), method=method))
}
