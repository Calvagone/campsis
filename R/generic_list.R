#_______________________________________________________________________________
#----                         pmx_list class                             ----
#_______________________________________________________________________________

#' 
#' PMX list class.
#' 
#' @export
setClass(
  "pmx_list",
  representation(
    list="list"
  ),
  prototype=prototype(list=list())
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

#' Add element to list.
#' 
#' @param object list object
#' @param x element to add
#' @return object
#' @export
add <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("add", function(object, x) {
  standardGeneric("add")
})

setMethod("add", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  if (validObject(x)) {
    if (object %>% contains(x)) {
      stop(paste("Element", x %>% getName(), "is already present."))
    } else {
      object@list <- c(object@list, x)
    }
  }
  return(object)
})

#_______________________________________________________________________________
#----                             replace                                   ----
#_______________________________________________________________________________

#' Replace element by another in list.
#' 
#' @param object list object
#' @param x element to replace
#' @return list object
#' @export
replace <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("replace", function(object, x) {
  standardGeneric("replace")
})

setMethod("replace", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  if (object %>% contains(x)) {
    index <- object %>% indexOf(x)
    object@list[[index]] <- x
  } else {
    stop(paste("Element", x %>% getName(), "does not exist."))
  }
  return(object)
})

#_______________________________________________________________________________
#----                             indexOf                                   ----
#_______________________________________________________________________________

#' Get the index of an element in list.
#' 
#' @param object list object
#' @param x element to know the index
#' @return index of this element
#' @export
indexOf <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("indexOf", function(object, x) {
  standardGeneric("indexOf")
})

setMethod("indexOf", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  logicalVector <- object@list %>% purrr::map_lgl(~(.x %>% getName()==x %>% getName()))
  index <- which(logicalVector)
  if (length(index) > 0) {
    index <- index[[1]]
  }
  return(index)
})

#_______________________________________________________________________________
#----                           getByName                                   ----
#_______________________________________________________________________________

#' Get an element from a list by name.
#' 
#' @param object list object
#' @param name element name to search for
#' @return index of this element
#' @export
getByName <- function(object, name) {
  stop("No default function is provided")
}

setGeneric("getByName", function(object, name) {
  standardGeneric("getByName")
})

setMethod("getByName", signature=c("pmx_list", "character"), definition=function(object, name) {
  element <- object@list %>% purrr::keep(~(.x %>% getName()==name))
  if (length(element) > 0) {
    element <- element[[1]]
  }
  return(element)
})

#_______________________________________________________________________________
#----                            contains                                   ----
#_______________________________________________________________________________

#' Check if an element exists in list.
#' 
#' @param object list object
#' @param x element to check if exists
#' @return logical value
#' @export
contains <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("contains", function(object, x) {
  standardGeneric("contains")
})

setMethod("contains", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  return(object %>% getByName(x %>% getName()) %>% length() != 0)
})

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

#' Get names of element in list.
#' 
#' @param object list object
#' @return character vector
#' @export
getNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getNames", function(object) {
  standardGeneric("getNames")
})

setMethod("getNames", signature=c("pmx_list"), definition=function(object) {
  return(object@list %>% purrr::map_chr(~.x %>% getName()))
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("pmx_list"), definition=function(x) {
  return(length(x@list))
})

#_______________________________________________________________________________
#----                         sort (ABSTRACT)                               ----
#_______________________________________________________________________________

# Reuse base::sort(x, decreasing = FALSE, ...) definition

#' Sort the specified list.
#' 
#' @param x list object
#' @param decreasing increasing or decreasing order
#' @param ... extra arguments
#' @return same list but ordered
#' @export
sort <- function(x, decreasing = FALSE, ...) {
  stop("No default function is provided")
}

setGeneric("sort", function(x, decreasing = FALSE, ...) {
  standardGeneric("sort")
})

#_______________________________________________________________________________
#----                       default (ABSTRACT)                              ----
#_______________________________________________________________________________

#' Get default element from list.
#' 
#' @param object list object
#' @param ... additional arguments
#' @return the default element from list
#' @export
default <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("default", function(object, ...) {
  standardGeneric("default")
})
