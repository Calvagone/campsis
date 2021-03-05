
#_______________________________________________________________________________
#----                            arm class                                  ----
#_______________________________________________________________________________

checkArm <- function(object) {
  return(expectOneForAll(object, c("id", "subjects")))
}

#' @export
setClass(
  "arm",
  representation(
    id = "integer",
    subjects = "integer",
    protocol = "protocol",
    covariates = "covariates"
  ),
  contains="pmx_element",
  prototype=prototype(id=as.integer(1), subjects=as.integer(1), protocol=new("protocol"), covariates=new("covariates"))
)

#' 
#' Create a treatment arm.
#' 
#' @param id arm unique ID, integer
#' @param subjects number of subjects in arm, integer
#' @return an arm  
#' @export
Arm <- function(id=1, subjects=1) {
  return(new("arm", id=as.integer(id), subjects=as.integer(subjects)))
}

#_______________________________________________________________________________
#----                           getName                                     ----
#_______________________________________________________________________________

setMethod("getName", signature = c("arm"), definition = function(x) {
  return(paste("ARM", x@id))
})

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________


setMethod("add", signature = c("arm", "treatment_entry"), definition = function(object, x) {
  object@protocol@treatment <- object@protocol@treatment %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "observation"), definition = function(object, x) {
  object@protocol@observations <- object@protocol@observations %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "covariate"), definition = function(object, x) {
  object@covariates <- object@covariates %>% add(x)
  return(object)
})

