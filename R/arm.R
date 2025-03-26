
#_______________________________________________________________________________
#----                            arm class                                  ----
#_______________________________________________________________________________

checkArm <- function(object) {
  return(expectOneForAll(object, c("id", "subjects", "label")))
}

getEmptyBootstrap <- function() {
  return(Bootstrap(data=data.frame(BS_ID=integer())))
}

#' 
#' Arm class.
#' 
#' @slot id arm unique ID, integer
#' @slot subjects number of subjects in arm, integer
#' @slot label arm label, single character string
#' @slot protocol protocol
#' @slot covariates covariates
#' @slot bootstrap covariates to be bootstrapped
#' @export
setClass(
  "arm",
  representation(
    id = "integer",
    subjects = "integer",
    label = "character",
    protocol = "protocol",
    covariates = "covariates",
    bootstrap = "bootstrap"
  ),
  contains="pmx_element",
  prototype=prototype(id=as.integer(NA), subjects=as.integer(1), label=as.character(NA),
                      protocol=new("protocol"), covariates=new("covariates"),
                      bootstrap=getEmptyBootstrap())
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
#----                           getCovariates                               ----
#_______________________________________________________________________________

#' @rdname getCovariates
setMethod("getCovariates", signature = c("arm"), definition = function(object) {
  return(object@covariates %>% getCovariates())
})

#_______________________________________________________________________________
#----                         getEventCovariates                            ----
#_______________________________________________________________________________

#' @rdname getEventCovariates
setMethod("getEventCovariates", signature = c("arm"), definition = function(object) {
  return(object@covariates %>% getEventCovariates())
})

#_______________________________________________________________________________
#----                         getFixedCovariates                            ----
#_______________________________________________________________________________

#' @rdname getFixedCovariates
setMethod("getFixedCovariates", signature = c("arm"), definition = function(object) {
  return(object@covariates %>% getFixedCovariates())
})

#_______________________________________________________________________________
#----                       getTimeVaryingCovariates                        ----
#_______________________________________________________________________________

#' @rdname getTimeVaryingCovariates
setMethod("getTimeVaryingCovariates", signature = c("arm"), definition = function(object) {
  return(object@covariates %>% getTimeVaryingCovariates())
})

#_______________________________________________________________________________
#----                              getIOVs                                  ----
#_______________________________________________________________________________

#' @rdname getIOVs
setMethod("getIOVs", signature = c("arm"), definition = function(object) {
  return(object@protocol@treatment@iovs)
})

#_______________________________________________________________________________
#----                            getOccasions                               ----
#_______________________________________________________________________________

#' @rdname getOccasions
setMethod("getOccasions", signature = c("arm"), definition = function(object) {
  return(object@protocol@treatment@occasions)
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
  # Note, we do not use add because add checks for uniqueness
  object@protocol@treatment@list <- object@protocol@treatment@list %>% append(x) 
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

setMethod("add", signature = c("arm", "bootstrap"), definition = function(object, x) {
  object@bootstrap <- x
  return(object)
})

#_______________________________________________________________________________
#----                               contains                                ----
#_______________________________________________________________________________

setMethod("contains", signature=c("arm", "pmx_element"), definition=function(object, x) {
  return(!is.null(object %>% find(x)))
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
#----                               find                                    ----
#_______________________________________________________________________________

setMethod("find", signature = c("arm", "treatment_entry"), definition = function(object, x) {
  return(object@protocol@treatment %>% find(x))
})

setMethod("find", signature = c("arm", "treatment_iov"), definition = function(object, x) {
  return(object@protocol@treatment %>% find(x))
})

setMethod("find", signature = c("arm", "occasion"), definition = function(object, x) {
  return(object@protocol@treatment %>% find(x))
})

setMethod("find", signature = c("arm", "dose_adaptation"), definition = function(object, x) {
  return(object@protocol@treatment %>% find(x))
})

setMethod("find", signature = c("arm", "observations"), definition = function(object, x) {
  return(object@protocol@observations %>% find(x))
})

setMethod("find", signature = c("arm", "covariate"), definition = function(object, x) {
  return(object@covariates %>% find(x))
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the number of subjects contained in this arm.
#' 
#' @param x arm
#' @return a number
setMethod("length", signature=c("arm"), definition=function(x) {
  return(x@subjects)
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

#_______________________________________________________________________________
#----                             setLabel                                  ----
#_______________________________________________________________________________

#' @rdname setLabel
#' @importFrom methods validObject
setMethod("setLabel", signature = c("arm", "character"), definition = function(object, x) {
  object@label <- x
  methods::validObject(object)
  return(object)
})

#_______________________________________________________________________________
#----                           setSubjects                                 ----
#_______________________________________________________________________________

#' @rdname setSubjects
#' @importFrom methods validObject
setMethod("setSubjects", signature = c("arm", "integer"), definition = function(object, x) {
  object@subjects <- x
  methods::validObject(object)
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("arm"), definition=function(object) {
  if (!is.na(object@id) && object@id != 0) {
    if (is.na(object@label)) {
      armLabel <- paste("Arm", object@id)
    } else {
      armLabel <- paste("Arm '", object@label, "'")
    }
    cat(paste0(armLabel, " (N=", object@subjects, ")"))
    cat("\n")
  }
  show(object@protocol)
  cat("\n")
  hasBootstrap <- !isTRUE(all.equal(object@bootstrap, getEmptyBootstrap()))
  hasCovariates <- length(object@covariates@list) > 0
  if (hasBootstrap) {
    if (hasCovariates) {
      # Only show covariates if not empty
      show(object@covariates)
    }
    show(object@bootstrap)
  } else {
    show(object@covariates) # Display 'No covariates' if empty list
  }
})

#_______________________________________________________________________________
#----                          unwrapTreatment                              ----
#_______________________________________________________________________________

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature = c("arm"), definition = function(object) {
  object@protocol@treatment <- object@protocol@treatment %>% unwrapTreatment()
  return(object)
})
