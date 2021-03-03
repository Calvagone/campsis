#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

#' Add element to list.
#' 
#' @param object object containing a list
#' @param x element to add
#' @return object
#' @export
add <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("add", function(object, x) {
  standardGeneric("add")
})

#_______________________________________________________________________________
#----                           hasElement                                  ----
#_______________________________________________________________________________

#' Check if an element exists in list.
#' 
#' @param object object containing a list
#' @param x element to check if exists
#' @return logical value
#' @export
hasElement <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("hasElement", function(object, x) {
  standardGeneric("hasElement")
})

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

#' Get names of element in list.
#' 
#' @param object object containing a list
#' @return character vector
#' @export
getNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getNames", function(object) {
  standardGeneric("getNames")
})

#_______________________________________________________________________________
#----                            length                                   ----
#_______________________________________________________________________________

# Already exists in base

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

