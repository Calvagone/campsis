
#_______________________________________________________________________________
#----                            arm class                                  ----
#_______________________________________________________________________________

checkArm <- function(object) {
  return(expectOneForAll(object, c("id", "subjects", "label")))
}

#' 
#' Arm class.
#' 
#' @slot id arm unique ID, integer
#' @slot subjects number of subjects in arm, integer
#' @slot label arm label, single character string
#' @slot protocol protocol
#' @slot covariates covariates
#' @export
setClass(
  "arm",
  representation(
    id = "integer",
    subjects = "integer",
    label = "character",
    protocol = "protocol",
    covariates = "covariates"
  ),
  contains="pmx_element",
  prototype=prototype(id=as.integer(NA), subjects=as.integer(1), label=as.character(NA),
                      protocol=new("protocol"), covariates=new("covariates"))
)

#'
#' Create a treatment arm.
#'
#' @param id unique identifier for this arm (available trough dataset), integer. If NA (default), this identifier is auto-incremented.
#' @param subjects number of subjects in arm, integer
#' @param label arm label, single character string. If set, this label will be output in the ARM column of CAMPSIS instead of the identifier.
#' @return an arm
#' @export
Arm <- function(id=as.integer(NA), subjects=1, label=as.character(NA)) {
  return(new("arm", id=as.integer(id), subjects=as.integer(subjects), label=as.character(label)))
}

#_______________________________________________________________________________
#----                          getCovariateNames                            ----
#_______________________________________________________________________________

#' @rdname getCovariateNames
setMethod("getCovariateNames", signature = c("arm"), definition = function(object) {
  return(object@covariates@list %>% purrr::map_chr(.f=~.x@name))
})

#_______________________________________________________________________________
#----                            getIOVNames                                ----
#_______________________________________________________________________________

#' @rdname getIOVNames
setMethod("getIOVNames", signature = c("arm"), definition = function(object) {
  return(object@protocol@treatment@iovs@list %>% purrr::map_chr(.f=~.x@colname))
})

#_______________________________________________________________________________
#----                         getOccasionNames                              ----
#_______________________________________________________________________________

#' @rdname getOccasionNames
setMethod("getOccasionNames", signature = c("arm"), definition = function(object) {
  return(object@protocol@treatment@occasions@list %>% purrr::map_chr(.f=~.x@colname))
})


#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

#' @rdname getTimeVaryingCovariateNames
setMethod("getTimeVaryingCovariateNames", signature = c("arm"), definition = function(object) {
  return((object@covariates %>% select("time_varying_covariate"))@list %>% purrr::map_chr(.f=~.x@name))
})

#_______________________________________________________________________________
#----                           getName                                     ----
#_______________________________________________________________________________

setMethod("getName", signature = c("arm"), definition = function(x) {
  return(paste("ARM", x@id))
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' @rdname getTimes
setMethod("getTimes", signature = c("arm"), definition = function(object) {
  return(object@protocol@observations %>% getTimes())
})

#_______________________________________________________________________________
#----                           add                                         ----
#_______________________________________________________________________________

setMethod("add", signature = c("arm", "list"), definition = function(object, x) {
  for (element in x) {
    object <- object %>% add(element)
  }
  return(object)
})

setMethod("add", signature = c("arm", "treatment_entry"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "treatment_iov"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "occasion"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "dose_adaptation"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "observations"), definition = function(object, x) {
  object@protocol@observations <- object@protocol@observations %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "covariate"), definition = function(object, x) {
  object@covariates <- object@covariates %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                              delete                                   ----
#_______________________________________________________________________________

setMethod("delete", signature = c("arm", "treatment_entry"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% delete(x) 
  return(object)
})

setMethod("delete", signature = c("arm", "treatment_iov"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% delete(x) 
  return(object)
})

setMethod("delete", signature = c("arm", "occasion"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% delete(x) 
  return(object)
})

setMethod("delete", signature = c("arm", "dose_adaptation"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% delete(x) 
  return(object)
})

setMethod("delete", signature = c("arm", "observations"), definition = function(object, x) {
  object@protocol@observations <- object@protocol@observations %>% delete(x) 
  return(object)
})

setMethod("delete", signature = c("arm", "covariate"), definition = function(object, x) {
  object@covariates <- object@covariates %>% delete(x)
  return(object)
})

#_______________________________________________________________________________
#----                             replace                                   ----
#_______________________________________________________________________________

setMethod("replace", signature = c("arm", "treatment_entry"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% replace(x) 
  return(object)
})

setMethod("replace", signature = c("arm", "treatment_iov"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% replace(x) 
  return(object)
})

setMethod("replace", signature = c("arm", "occasion"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% replace(x) 
  return(object)
})

setMethod("replace", signature = c("arm", "dose_adaptation"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% replace(x) 
  return(object)
})

setMethod("replace", signature = c("arm", "observations"), definition = function(object, x) {
  object@protocol@observations <- object@protocol@observations %>% replace(x) 
  return(object)
})

setMethod("replace", signature = c("arm", "covariate"), definition = function(object, x) {
  object@covariates <- object@covariates %>% replace(x)
  return(object)
})
