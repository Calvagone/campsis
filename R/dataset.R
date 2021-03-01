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