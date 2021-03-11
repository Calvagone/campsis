
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
#' Create a covariate.
#' 
#' @param name covariate name, character
#' @param distribution covariate distribution
#' @return a covariate  
#' @export
Covariate <- function(name, distribution) {
  return(new("covariate", name=name, distribution=distribution))
}

#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

setMethod("sample", signature = c("covariate", "integer"), definition = function(object, n) {
  object@distribution <- object@distribution %>% sample(n)
  return(object)
})


