
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
    default_arm = "arm",
    arms = "arms"
  ),
  prototype=prototype(default_arm=new('arm', id=as.integer(0), subjects=as.integer(1)), arms=new("arms"))
)

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("dataset", "arm"), definition = function(object, x) {
  object@arms <- object@arms %>% add(x) 
  return(object)
}
)

setMethod("add", signature = c("dataset", "treatment_entry"), definition = function(object, x) {
  object@default_arm@protocol@treatment <- object@default_arm@protocol@treatment %>% add(x) 
    return(object)
  }
)

setMethod("add", signature = c("dataset", "observation"), definition = function(object, x) {
  object@default_arm@protocol@observations <- object@default_arm@protocol@observations %>% add(x) 
  return(object)
}
)

setMethod("add", signature = c("dataset", "covariate"), definition = function(object, x) {
  object@default_arm@covariates <- object@default_arm@covariates %>% add(x)
  return(object)
})

#' #_______________________________________________________________________________
#' #----                                 filter                                ----
#' #_______________________________________________________________________________
#' 
#' #' Filter.
#' #' 
#' #' @param object generic object
#' #' @param x element used as filter
#' #' @return filtered object
#' #' @export
#' filter <- function(object, x) {
#'   stop("No default function is provided")
#' }
#' 
#' setGeneric("filter", function(object, x) {
#'   standardGeneric("filter")
#' })
#' 
#' setMethod("filter", signature=c("dataset", "character"), definition=function(object, x) {
#'   object@entries <- object@entries %>% purrr::keep(~is(.x, x))
#'   return(object)
#' })
#' 
#' setMethod("filter", signature=c("dataset", "arm"), definition=function(object, x) {
#'   object@entries <- object@entries %>% purrr::keep(~(x@id %in% (.x@arms %>% purrr::map_int(~.x@id))))
#'   return(object)
#' })


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

  # Use either arms or default_arm
  arms <- object@arms
  if (length(arms) == 0) {
    arms = new("arms")
    arms <- arms %>% add(object@default_arm)
  }
  
  # Compute max ID per arm
  maxIDPerArm <- arms@list %>% purrr::map_int(~.x@subjects) %>% purrr::accumulate(~(.x+.y))
  
  retValue <- purrr::map2_df(arms@list, maxIDPerArm, .f=function(arm, maxID) {
    armID <- arm@id
    subjects <- arm@subjects
    protocol <- arm@protocol
    treatment <- protocol@treatment
    observations <- protocol@observations
    
    # Fill in entries list
    entries <- new("time_entries")
    treatment@list %>% purrr::map(.f=function(entry) {
      entries <<- entries %>% add(entry)
    })
    observations@list %>% purrr::map(.f=function(entry) {
      entries <<- entries %>% add(entry)
    })
    
    # Order
    entries <- entries %>% order()

    # Interesting part
    df <- entries@list %>% purrr::map_df(.f=~convert(.x))

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