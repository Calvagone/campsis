
#_______________________________________________________________________________
#----                        covariate class                                ----
#_______________________________________________________________________________

checkCovariate <- function(object) {
  return(expectOneForAll(object, c("name")))
}

#' 
#' Covariate entry class.
#' 
#' @export
setClass(
  "covariate",
  representation(
    name = "character"
  ),
  validity=checkCovariate
)

#_______________________________________________________________________________
#----                     constant_covariate class                          ----
#_______________________________________________________________________________

checkConstantCovariate <- function(object) {
  return(expectOneForAll(object, c("value")))
}

#' 
#' Covariate entry class.
#' 
#' @export
setClass(
  "constant_covariate",
  representation(
    value = "numeric"
  ),
  contains="covariate",
  validity=checkConstantCovariate
)

#_______________________________________________________________________________
#----                       fixed_covariate class                           ----
#_______________________________________________________________________________

checkFixedCovariate <- function(object) {
  check <- expectOneOrMore(object, c("values"))
  return(checkReturn(check))
}

#' 
#' Covariate entry class.
#' 
#' @export
setClass(
  "fixed_covariate",
  representation(
    values = "numeric"
  ),
  contains="covariate",
  validity=checkFixedCovariate
)

#_______________________________________________________________________________
#----                     function_covariate class                          ----
#_______________________________________________________________________________

checkFunctionCovariate <- function(object) {
  return(checkReturn(character(0)))
}

#' 
#' Covariate entry class.
#' 
#' @export
setClass(
  "function_covariate",
  representation(
    fun = "function"
  ),
  contains="fixed_covariate",
  prototype=prototype(values=numeric(0)),
  validity=checkFunctionCovariate
)

#_______________________________________________________________________________
#----                     bootstrap_covariate class                         ----
#_______________________________________________________________________________

checkBoostrapCovariate <- function(object) {
  return(expectOneForAll(object, c("replacement", "random")))
}

#' 
#' Function covariate class.
#' 
#' @export
setClass(
  "boostrap_covariate",
  representation(
    data = "numeric",
    replacement = "logical",
    random = "logical"
  ),
  contains="fixed_covariate",
  prototype=prototype(values=numeric(0), replacement=FALSE, random=FALSE),
  validity=checkBoostrapCovariate
)


