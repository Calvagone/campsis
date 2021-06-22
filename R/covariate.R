
#_______________________________________________________________________________
#----                        covariate class                                ----
#_______________________________________________________________________________

checkCovariate <- function(object) {
  return(expectOneForAll(object, c("name", "distribution")))
}

#' @export
setClass(
  "covariate",
  representation(
    name = "character",
    distribution = "distribution"
  ),
  contains="pmx_element",
  validity=checkCovariate
)

setMethod("getName", signature = c("covariate"), definition = function(x) {
  return(paste0("COVARIATE [", "NAME=", x@name, "]"))
})

#' 
#' Create a fixed covariate.
#' 
#' @param name covariate name, character
#' @param distribution covariate distribution
#' @return a fixed covariate  
#' @export
Covariate <- function(name, distribution) {
  return(new("covariate", name=name, distribution=toExplicitDistribution(distribution)))
}

#_______________________________________________________________________________
#----                     time_varying_covariate class                      ----
#_______________________________________________________________________________

#' @export
setClass(
  "time_varying_covariate",
  representation(
  ),
  contains="covariate"
)

#' 
#' Create a time-varying covariate.
#' 
#' @param name covariate name, character
#' @param distribution covariate distribution at time 0
#' @return a time-varying covariate  
#' @export
TimeVaryingCovariate <- function(name, distribution) {
  return(new("time_varying_covariate", name=name, distribution=toExplicitDistribution(distribution)))
}

#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

#' @rdname sample
setMethod("sample", signature = c("covariate", "integer"), definition = function(object, n) {
  object@distribution <- object@distribution %>% sample(n)
  return(object)
})

