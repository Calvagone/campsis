
#_______________________________________________________________________________
#----                     dataset_entry class                               ----
#_______________________________________________________________________________

checkDatasetEntry <- function(object) {
  return(checkReturn(checkObject(object, "time")))
}

setClass(
  "dataset_entry",
  representation(
    time = "numeric"
  ),
  validity=checkDatasetEntry
)

#_______________________________________________________________________________
#----                            arm class                                  ----
#_______________________________________________________________________________

checkArm <- function(object) {
  return(checkObject(object, c("id", "subjects")))
}

#' 
#' Arm class.
#' 
#' @export
setClass(
  "arm",
  representation(
    id = "integer",
    subjects = "integer"
  ),
  prototype=prototype(id=as.integer(1), subjects=as.integer(1))
)

#_______________________________________________________________________________
#----                     treatment_entry class                             ----
#_______________________________________________________________________________

checkArms <- function(object) {
  errors <- character()
  if (length(object@arms) != 0) {
    for(arm in object@arms) {
      errors <- addError(checkArm(arm), errors)
    }
  }
  return(errors)
}

checkTreatmentEntry <- function(object) {
  check1 <- checkObject(object, c("amount", "compartment"))
  check2 <- checkArms(object)
  return(checkReturn(c(check1, check2)))
}

#' 
#' Treatment entry class.
#' 
setClass(
  "treatment_entry",
  representation(
    amount = "numeric",
    compartment = "integer",
    arms = "list"
  ),
  contains = "dataset_entry",
  prototype=prototype(compartment=as.integer(NA), arms=list()),
  validity=checkTreatmentEntry
)

#_______________________________________________________________________________
#----                           bolus class                                 ----
#_______________________________________________________________________________

checkBolus <- function(object) {
  return(TRUE)
}

#' 
#' Bolus class.
#' 
#' @export
setClass(
  "bolus",
  representation(
  ),
  contains = "treatment_entry",
  validity=checkBolus
)

#_______________________________________________________________________________
#----                        infusion class                                 ----
#_______________________________________________________________________________

checkInfusion <- function(object) {
  check1 <- checkObject(object, c("duration"))
  check2 <- checkArms(object)
  return(checkReturn(c(check1, check2)))
}

#' 
#' Infusion class.
#' 
#' @export
setClass(
  "infusion",
  representation(
    duration = "numeric"
  ),
  contains = "treatment_entry",
  validity=checkInfusion
)

#_______________________________________________________________________________
#----                     observation class                                ----
#_______________________________________________________________________________

checkObservation <- function(object) {
  check1 <- checkObject(object, c("compartment"))
  check2 <- checkArms(object)
  return(checkReturn(c(check1, check2)))
}

#' 
#' Observation entry class.
#' 
#' @export
setClass(
  "observation",
  representation(
    compartment = "integer",
    arms = "list"
  ),
  contains = "dataset_entry",
  prototype=prototype(compartment=as.integer(NA), arms=list()),
  validity=checkObservation
)

#' 
#' Dataset class.
#' 
#' @export
setClass(
  "dataset",
  representation(
    entries = "list"
  ),
  prototype=prototype(entries=list())
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
#----                           add                                   ----
#_______________________________________________________________________________

#' Add entry to list.
#' 
#' @param object generic object
#' @param entry entry to add
#' @return generic object
#' @export
add <- function(object, entry) {
  stop("No default function is provided")
}

setGeneric("add", function(object, entry) {
  standardGeneric("add")
})

setMethod("add", signature = c("dataset", "dataset_entry"), definition = function(object, entry) {
    object@entries <- c(object@entries, entry)
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
  object@entries <- object@entries %>% purrr::keep(~is(.x, type))
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
  types <- object@entries %>% purrr::map_chr(~as.character(class(.x)))
  times <- object@entries %>% purrr::map_dbl(~.x@time)

  # Reorder
  types <- factor(types, levels=c("observation", "bolus", "infusion"), labels=c("observation", "bolus", "infusion"))
  order <- base::order(times, types)
  
  # Apply result to original list
  object@entries <- object@entries[order]
  return(object)
})

#_______________________________________________________________________________
#----                              getArms                                  ----
#_______________________________________________________________________________

#' Get arms from dataset.
#'
#' @param object generic object
#' @return a list of arms
#' @export
getArms <- function(object) {
  stop("No default function is provided")
}

setGeneric("getArms", function(object) {
  standardGeneric("getArms")
})

setMethod("getArms", signature=c("dataset"), definition=function(object) {
  armIds <- NULL
  retValue <- NULL
  object@entries %>% purrr::map(.f=function(entry) {
    arms <- entry@arms
    for (arm in arms) {
      if (!(arm@id %in% armIds)) {
        armIds <<- c(armIds, arm@id)
        retValue <<- c(retValue, arm)
      }
    }
  })
  return(retValue)
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