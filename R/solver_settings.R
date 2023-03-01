#_______________________________________________________________________________
#----                          solver_settings class                        ----
#_______________________________________________________________________________

#' 
#' Solver settings class.
#' See ?mrgsolve::update.
#' See ?rxode2::rxSolve.
#' 
#' @slot atol absolute solver tolerance, default is 1e-08
#' @slot rtol relative solver tolerance, default is 1e-08
#' @slot hmax limit how big a solver step can be, default is NA
#' @slot maxsteps max steps between 2 integration times (e.g. when observations records are far apart), default is 70000
#' @slot method solver method, for RxODE/rxode2 only: 'liblsoda' (default), 'lsoda', 'dop853', 'indLin'. Mrgsolve's method is always 'lsoda'.
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
#' @param atol absolute solver tolerance, default is 1e-08
#' @param rtol relative solver tolerance, default is 1e-08
#' @param hmax limit how big a solver step can be, default is NA
#' @param maxsteps max steps between 2 integration times (e.g. when observations records are far apart), default is 70000
#' @param method solver method, for RxODE/rxode2 only: 'liblsoda' (default), 'lsoda', 'dop853', 'indLin'. Mrgsolve's method is always 'lsoda'.
#'
#' @return solver settings
#' @export
Solver <- function(atol=1e-08, rtol=1e-08, hmax=NA, maxsteps=70000L, method="liblsoda") {
  return(new("solver_settings", atol=atol, rtol=rtol, hmax=as.numeric(hmax), maxsteps=as.integer(maxsteps), method=method))
}
