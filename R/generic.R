#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' Export type class.
#' 
#' @export
setClass(
  "export_type",
  representation(
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Export function.
#' 
#' @param object generic object
#' @param dest destination
#' @return specific object depending on given destination
#' @export
export <- function(object, dest) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest) {
  standardGeneric("export")
})