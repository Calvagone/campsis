#_______________________________________________________________________________
#----                         declare_settings class                        ----
#_______________________________________________________________________________

#' 
#' Declare settings class.
#' 
#' @slot variables uninitialized variables to be declared, only needed with mrgsolve
#' @export
setClass(
  "declare_settings",
  representation(
    variables="character"
  ),
  prototype=prototype(variables=character(0))
)

#'
#' Create declare settings.
#'
#' @param variables uninitialized variables to be declared, only needed with mrgsolve
#' @return Declare settings
#' @export
Declare <- function(variables=character(0)) {
  return(new("declare_settings", variables=variables))
}
