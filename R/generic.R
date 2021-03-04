
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
#' @param ... optional arguments
#' @return specific object depending on given destination
#' @export
export <- function(object, dest, ...) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest, ...) {
  standardGeneric("export")
})

#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________

#' Convert generic object according to given configuration.
#' 
#' @param object generic object
#' @param config specified configuration
#' @return conversion output
#' @export
convert <- function(object, config) {
  stop("No default function is provided")
}

setGeneric("convert", function(object, config) {
  standardGeneric("convert")
})

