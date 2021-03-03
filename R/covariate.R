
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
    args = "list"
  ),
  contains="abstract_fixed_covariate",
  prototype=prototype(args=list()),
  validity=checkFunctionCovariate
)

#_______________________________________________________________________________
#----                     bootstrap_covariate class                         ----
#_______________________________________________________________________________

checkBootstrapCovariate <- function(object) {
  check1 <- expectOneOrMore(object, c("data"))
  check2 <- expectOneForAll(object, c("replacement", "random"))
  return(c(check1, check2))
}

#' 
#' Bootstrap covariate class.
#' 
#' @export
setClass(
  "bootstrap_covariate",
  representation(
    data = "numeric",
    replacement = "logical",
    random = "logical"
  ),
  contains="abstract_fixed_covariate",
  prototype=prototype(replacement=FALSE, random=FALSE),
  validity=checkBootstrapCovariate
)

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' Sample generic object.
#' 
#' @param object generic object
#' @return sampling result
#' @export
sample <- function(object, n) {
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
  SAMPLING_SIZE <- n
  
  # If "n" is provided as value in args, this means the first arg of the function
  # is not related to the size
  hasNCharAsValue <- "n" %in% as.character(args)
  
  if (length(args) == 0) {
    args_str <- "SAMPLING_SIZE"
  } else {
    # Example: return 'mean=70, sd=2'
    args_str <- purrr::accumulate2(names(args), args, .f=function(.x, .y, .z){
      comma <- if (.x=="") {""} else {", "}
      if (.z=="n") {
        .z <- "SAMPLING_SIZE"
      }
      paste0(.x, comma, .y, "=", .z)
    }, .init="")
    
    if (hasNCharAsValue) {
      # Size is not the first argument
      args_str <- args_str[length(args_str)]
    } else {
      # Size is the first argument
      args_str <- paste0("SAMPLING_SIZE, ", args_str[length(args_str)])
    }
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

setMethod("sample", signature = c("bootstrap_covariate", "integer"), definition = function(object, n) {
  data <- object@data
  nData <- length(data)
  replacement <- object@replacement
  random <- object@random
  
  if (n > nData && !replacement) {
    stop(paste("Covariate", object@name, "does not have enough data to generate", n, "samples (please set 'replacement' to TRUE)"))
  }
  
  if (random) {
    values <- data[sample.int(n=nData, size=n, replace=replacement)]
  } else {
    values <- rep(data, length.out=n)
  }
  
  # Assign values
  object@values <- values
  return(object)
})

