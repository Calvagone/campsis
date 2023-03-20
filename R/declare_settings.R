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

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("declare_settings"), definition=function(object) {
  if (identical(object, Declare())) {
    cat("")  
  } else {
    cat(sprintf("Declare: variables={%s}", paste0(object@variables, collapse=", ")))
    cat("\n")
  }
})
