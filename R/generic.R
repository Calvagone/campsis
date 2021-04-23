#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________

#' #' Convert generic object according to given configuration.
#' #' 
#' #' @param object generic object
#' #' @param config specified configuration
#' #' @return conversion output
#' #' @export
#' convert <- function(object, config) {
#'   stop("No default function is provided")
#' }
#' 
#' setGeneric("convert", function(object, config) {
#'   standardGeneric("convert")
#' })

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
#' @param ... extra arguments
#' @return sampling result
#' @export
sample <- function(object, n, ...) {
  stop("No default function is provided")
}

setGeneric("sample", function(object, n, ...) {
  standardGeneric("sample")
})

#_______________________________________________________________________________
#----                         getColumnName                                 ----
#_______________________________________________________________________________

#' Get column name.
#' 
#' @param x any object
#' @return column name
#' @export
getColumnName <- function(x) {
  stop("No default function is provided")
}

setGeneric("getColumnName", function(x) {
  standardGeneric("getColumnName")
})

#_______________________________________________________________________________
#----                           getIOVNames                                 ----
#_______________________________________________________________________________

#' Get IOV names.
#' 
#' @param object any object
#' @return character vector
#' @export
getIOVNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getIOVNames", function(object) {
  standardGeneric("getIOVNames")
})

#_______________________________________________________________________________
#----                         getCovariateNames                             ----
#_______________________________________________________________________________

#' Get covariate names.
#' 
#' @param object any object
#' @return character vector
#' @export
getCovariateNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getCovariateNames", function(object) {
  standardGeneric("getCovariateNames")
})

