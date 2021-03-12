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

#_______________________________________________________________________________
#----                         getByCompartment                              ----
#_______________________________________________________________________________

#' Get element by compartment number.
#' 
#' @param object any object
#' @param compartment compartment number
#' @return the element that has the right compartment
#' @export
getByCompartment <- function(object, compartment) {
  stop("No default function is provided")
}

setGeneric("getByCompartment", function(object, compartment) {
  standardGeneric("getByCompartment")
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
#----                      hasParameterDistribution                         ----
#_______________________________________________________________________________

#' Tell if the dataset contains at least one parameter distribution (in its treatment characteristics).
#' This is useful information to know as we therefore need the PMX model to know the
#' corresponding values (THETA and OMEGA) for this parameter.
#' 
#' @param object generic object
#' @return logical value
#' @export
hasParameterDistribution <- function(object) {
  stop("No default function is provided")
}

setGeneric("hasParameterDistribution", function(object) {
  standardGeneric("hasParameterDistribution")
})

