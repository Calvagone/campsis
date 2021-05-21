
#_______________________________________________________________________________
#----                        covariate class                                ----
#_______________________________________________________________________________

checkCovariate <- function(object) {
  return(expectOneForAll(object, c("name", "distribution", "time_varying")))
}

#' @export
setClass(
  "covariate",
  representation(
    name = "character",
    distribution = "distribution",
    time_varying = "logical"
  ),
  contains="pmx_element",
  prototype=prototype(time_varying=FALSE),
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

#' 
#' Create a time-varying covariate.
#' 
#' @param name covariate name, character
#' @param distribution covariate distribution at time 0
#' @return a time-varying covariate  
#' @export
TimeVaryingCovariate <- function(name, distribution) {
  return(new("covariate", name=name, distribution=toExplicitDistribution(distribution), time_varying=TRUE))
}

#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

setMethod("sample", signature = c("covariate", "integer"), definition = function(object, n) {
  object@distribution <- object@distribution %>% sample(n)
  return(object)
})


