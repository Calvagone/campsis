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
#----                             simulate                                  ----
#_______________________________________________________________________________

#' Simulate function.
#' 
#' @param model generic model
#' @param dataset generic dataset
#' @param dest destination simulation engine
#' @param ... optional arguments
#' @return specific object depending on given destination
#' @export
simulate <- function(object, dataset, dest, ...) {
  stop("No default function is provided")
}

setGeneric("simulate", function(model, dataset, dest, ...) {
  standardGeneric("simulate")
})

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' Sample generic object.
#' 
#' @param object generic object
#' @param n number of samples required
#' @return sampling result
#' @export
sample <- function(object, n) {
  stop("No default function is provided")
}

setGeneric("sample", function(object, n) {
  standardGeneric("sample")
})

