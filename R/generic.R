
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
#' @rdname sample
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
#' @rdname getColumnName
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
#' @rdname getIOVNames
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
#' @rdname getCovariateNames
getCovariateNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getCovariateNames", function(object) {
  standardGeneric("getCovariateNames")
})

#_______________________________________________________________________________
#----                          getOccasionNames                             ----
#_______________________________________________________________________________

#' Get all occasion names.
#' 
#' @param object any object
#' @return character vector
#' @export
#' @rdname getOccasionNames
getOccasionNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getOccasionNames", function(object) {
  standardGeneric("getOccasionNames")
})

#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

#' Get all time-varying covariate names.
#' 
#' @param object any object
#' @return character vector
#' @export
#' @rdname getTimeVaryingCovariateNames
getTimeVaryingCovariateNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getTimeVaryingCovariateNames", function(object) {
  standardGeneric("getTimeVaryingCovariateNames")
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' Get all distinct times for the specified object.
#' 
#' @param object any object
#' @return numeric vector with all unique times, sorted
#' @export
#' @rdname getTimes
getTimes <- function(object) {
  stop("No default function is provided")
}

setGeneric("getTimes", function(object) {
  standardGeneric("getTimes")
})
