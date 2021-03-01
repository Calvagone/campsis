#' 
#' Dataset entry class.
#' 
setClass(
  "dataset_entry",
  representation(
    time = "numeric"
  )
)

#' 
#' Treatment entry class.
#' 
setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer"
  ),
  contains = "dataset_entry",
  prototype=prototype(compartment=as.integer(NA))
)

#' 
#' Bolus class.
#' 
#' @export
setClass(
  "bolus",
  representation(
  ),
  contains = "treatment_entry"
)

#' 
#' Infusion class.
#' 
#' @export
setClass(
  "infusion",
  representation(
    duration = "numeric"
  ),
  contains = "treatment_entry"
)

#' 
#' Observation entry class.
#' 
#' @export
setClass(
  "observation",
  representation(
    compartment = "integer"
  ),
  contains = "dataset_entry",
  prototype=prototype(compartment=as.integer(NA))
)

#' 
#' Dataset class.
#' 
#' @export
setClass(
  "dataset",
  representation(
    list = "list"
  )
)

#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________

#' Convert dataset entry.
#' 
#' @param object generic object
#' @param entry entry to add
#' @return generic object
#' @export
convert <- function(object) {
  stop("No default function is provided")
}

setGeneric("convert", function(object) {
  standardGeneric("convert")
})

setMethod("convert", signature = c("bolus"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=1, MDV=1, DV=NA, AMT=object@amount, RATE=0, CMT=object@compartment))
})

#_______________________________________________________________________________
#----                           addEntry                                    ----
#_______________________________________________________________________________

#' Add entry to list.
#' 
#' @param object generic object
#' @param entry entry to add
#' @return generic object
#' @export
addEntry <- function(object, entry) {
  stop("No default function is provided")
}

setGeneric("addEntry", function(object, entry) {
  standardGeneric("addEntry")
})

setMethod("addEntry", signature = c("dataset", "dataset_entry"), definition = function(object, entry) {
    object@list <- c(object@list, entry)
    return(object)
  }
)

#_______________________________________________________________________________
#----                                 filter                                ----
#_______________________________________________________________________________

#' Filter.
#' 
#' @param object generic object
#' @param type parameter type: theta, omega or sigma
#' @return filtered object
#' @export
filter <- function(object, type) {
  stop("No default function is provided")
}

setGeneric("filter", function(object, type) {
  standardGeneric("filter")
})

setMethod("filter", signature=c("dataset", "character"), definition=function(object, type) {
  object@list <- object@list %>% purrr::keep(~as.character(class(.x))==type)
  return(object)
})

#_______________________________________________________________________________
#----                                  order                                ----
#_______________________________________________________________________________

#' Order.
#' 
#' @param object generic object
#' @return ordered object
#' @export
order <- function(object) {
  stop("No default function is provided")
}

setGeneric("order", function(object) {
  standardGeneric("order")
})

setMethod("order", signature=c("dataset"), definition=function(object) {
  types <- object@list %>% purrr::map_chr(~as.character(class(.x)))
  times <- object@list %>% purrr::map_dbl(~.x@time)

  # Reorder
  types <- factor(types, levels=c("observation", "bolus", "infusion"), labels=c("observation", "bolus", "infusion"))
  order <- base::order(times, types)
  
  # Apply result to original list
  object@list <- object@list[order]
  return(object)
})

#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' RxODE export type class.
#' 
#' @export
setClass(
  "rxode_type",
  representation(
  ),
  contains="export_type" 
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________


setMethod("export", signature=c("dataset", "character"), definition=function(object, dest) {
  if (dest=="RxODE") {
    return(object %>% export(new("rxode_type")))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("export", signature=c("dataset", "rxode_type"), definition=function(object, dest) {
  return(data.frame())
})