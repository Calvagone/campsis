
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
#----                 abstract_fixed_covariate class                        ----
#_______________________________________________________________________________

checkAbstractFixedCovariate <- function(object) {
  return(expectZeroOrMore(object, c("values")))
}

#' 
#' Abstract fixed covariate entry class.
#' 
setClass(
  "abstract_fixed_covariate",
  representation(
    values = "numeric"
  ),
  contains="covariate",
  prototype=prototype(values=numeric(0)),
  validity=checkAbstractFixedCovariate
)

#_______________________________________________________________________________
#----                       fixed_covariate class                           ----
#_______________________________________________________________________________

checkFixedCovariate <- function(object) {
  return(expectOneOrMore(object, c("values")))
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
  contains="abstract_fixed_covariate",
  validity=checkFixedCovariate
)

#_______________________________________________________________________________
#----                     function_covariate class                          ----
#_______________________________________________________________________________

checkFunctionCovariate <- function(object) {
  check1 <- expectOne(object, "fun")
  check2 <- expectZeroOrMore(object, "args")
  return(c(check1, check2))
}

#' 
#' Covariate entry class.
#' 
#' @export
setClass(
  "function_covariate",
  representation(
    fun = "character",
    args = "numeric"
  ),
  contains="abstract_fixed_covariate",
  prototype=prototype(args=numeric(0)),
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
  contains="abstract_fixed_covariate",
  prototype=prototype(replacement=FALSE, random=FALSE),
  validity=checkBoostrapCovariate
)

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' Sample generic object.
#' 
#' @param object generic object
#' @return sampling result
#' @export
sample <- function(object) {
  stop("No default function is provided")
}

setGeneric("sample", function(object, n) {
  standardGeneric("sample")
})

sample_delegate <- function(fun, n, ...) {
  return(fun(n, ...))
}

setMethod("sample", signature = c("function_covariate", "integer"), definition = function(object, n) {
  fun <- get(object@fun)
  args <- object@args
  
  if (length(args) == 0) {
    args_str <- "n"
  } else {
    # Example: return 'mean=70, sd=2'
    args_str <- purrr::accumulate2(names(args), as.numeric(args), .f=function(.x, .y, .z){
      comma <- if (.x=="") {""} else {", "}
      paste0(.x, comma, .y, "=", .z)
    }, .init="")
    args_str <- paste0("n, ", args_str[length(args_str)])
  }
  
  # Eval string expression
  text <- paste0("sample_delegate(fun, ", args_str, ")")
  values <- eval(parse(text=text))
  
  # Check result
  if (length(values) != n) {
    stop(paste("Calling function", object@fun, "with arguments", args_str, "does not provided", n, "values but", length(values)))
  }
  
  # Assign values
  object@values <- values
  
  return(object)
})

