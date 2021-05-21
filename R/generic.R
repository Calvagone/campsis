
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

#' Get all covariate names (fixed covariates + time-varying covariates).
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

#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

#' Get all time-varying covariate names.
#' 
#' @param object any object
#' @return character vector
#' @export
getTimeVaryingCovariateNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getTimeVaryingCovariateNames", function(object) {
  standardGeneric("getTimeVaryingCovariateNames")
})

