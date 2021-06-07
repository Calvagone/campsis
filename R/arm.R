
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
#----                          getCovariateNames                            ----
#_______________________________________________________________________________

setMethod("getCovariateNames", signature = c("arm"), definition = function(object) {
  return(object@covariates@list %>% purrr::map_chr(.f=~.x@name))
})

#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

setMethod("getTimeVaryingCovariateNames", signature = c("arm"), definition = function(object) {
  return((object@covariates %>% pmxmod::select("time_varying_covariate"))@list %>% purrr::map_chr(.f=~.x@name))
})

#_______________________________________________________________________________
#----                            getIOVNames                                ----
#_______________________________________________________________________________

setMethod("getIOVNames", signature = c("arm"), definition = function(object) {
  return(object@protocol@treatment@iovs@list %>% purrr::map_chr(.f=~.x@colname))
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

setMethod("getTimes", signature = c("arm"), definition = function(object) {
  return(object@protocol@observations %>% getTimes())
})

#_______________________________________________________________________________
#----                           add                                   ----
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

setMethod("add", signature = c("arm", "observations"), definition = function(object, x) {
  object@protocol@observations <- object@protocol@observations %>% add(x) 
  return(object)
})

setMethod("add", signature = c("arm", "covariate"), definition = function(object, x) {
  object@covariates <- object@covariates %>% add(x)
  return(object)
})

