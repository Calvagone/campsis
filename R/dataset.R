
#_______________________________________________________________________________
#----                         dataset class                                 ----
#_______________________________________________________________________________

#' 
#' Dataset class.
#' 
#' @export
setClass(
  "dataset",
  representation(
    entries = "list",
    covariates = "covariates"
  ),
  prototype=prototype(entries=list(), covariates=new("covariates"))
)


#_______________________________________________________________________________
#----                            convert                                    ----
#_______________________________________________________________________________

#' Convert generic object.
#' 
#' @param object generic object
#' @return conversion output
#' @export
convert <- function(object) {
  stop("No default function is provided")
}

setGeneric("convert", function(object) {
  standardGeneric("convert")
})

setMethod("convert", signature = c("bolus"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.integer(0), CMT=object@compartment))
})

setMethod("convert", signature = c("infusion"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=as.integer(1), MDV=as.integer(1), DV=".", AMT=object@amount, RATE=as.integer(-2), CMT=object@compartment))
})

setMethod("convert", signature = c("observation"), definition = function(object) {
  return(data.frame(TIME=object@time, EVID=as.integer(0), MDV=as.integer(0), DV=".", AMT=as.numeric(NA), RATE=as.integer(NA), CMT=object@compartment))
})

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("dataset", "dataset_entry"), definition = function(object, x) {
    object@entries <- c(object@entries, x)
    return(object)
  }
)

setMethod("add", signature = c("dataset", "covariate"), definition = function(object, x) {
  object@covariates <- object@covariates %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                                 filter                                ----
#_______________________________________________________________________________

#' Filter.
#' 
#' @param object generic object
#' @param x element used as filter
#' @return filtered object
#' @export
filter <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("filter", function(object, x) {
  standardGeneric("filter")
})

setMethod("filter", signature=c("dataset", "character"), definition=function(object, x) {
  object@entries <- object@entries %>% purrr::keep(~is(.x, x))
  return(object)
})

setMethod("filter", signature=c("dataset", "arm"), definition=function(object, x) {
  object@entries <- object@entries %>% purrr::keep(~(x@id %in% (.x@arms %>% purrr::map_int(~.x@id))))
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
  retValue <- list()
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
  arms <- object %>% getArms()
  noArm <- length(arms)==0
  if (noArm) {
    arms <- list(new("arm"))
  }
  object <- object %>% order()
  maxIDPerArm <- arms %>% purrr::map_int(~.x@subjects) %>% purrr::accumulate(~(.x+.y))
  
  retValue <- purrr::map2_df(arms, maxIDPerArm, .f=function(arm, maxID) {
    armID <- arm@id
    subjects <- arm@subjects

    # Filter or not according to noArm
    if (noArm) {
      subObject <- object
    } else {
      subObject <- object %>% filter(arm)
    }
    
    # Interesting part
    df <- subObject@entries %>% purrr::map_df(.f=~convert(.x))

    # Replicating part
    ids <- seq_len(subjects) + maxID - subjects
    expandedDf <- ids %>% purrr::map_df(.f=function(id) {
      df <- df %>% tibble::add_column(ID=id, .before="TIME")
      df <- df %>% tibble::add_column(ARM=armID, .before="TIME")
    })
    
    return(expandedDf)
  })
  
  return(retValue)
})