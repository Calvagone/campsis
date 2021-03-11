#_______________________________________________________________________________
#----                        distribution class                             ----
#_______________________________________________________________________________

validateDistribution <- function(object) {
  return(TRUE)
}

#' @export
setClass(
  "distribution",
  representation(
  ),
  contains="pmx_element",
  validity=validateDistribution
)

#_______________________________________________________________________________
#----                   parameter_distribution class                        ----
#_______________________________________________________________________________

validateParameterDistribution <- function(object) {
  check1 <- expectOne(object, "theta_name")
  check2 <- expectZeroOrOne(object, "eta_name")
  return(c(check1, check2))
}

#' @export
setClass(
  "parameter_distribution",
  representation(
    theta_name = "character",
    eta_name = "character"
  ),
  contains="distribution",
  validity=validateParameterDistribution
)

#' 
#' Create a parameter distribution.
#' 
#' @param thetaName corresponding THETA name, character
#' @param etaName corresponding ETA name, character (character(0) if no ETA)
#' @return a parameter distribution  
#' @export
ParameterDistribution <- function(thetaName, etaName=character(0)) {
  return(new("parameter_distribution", theta_name=thetaName, eta_name=etaName))
}

#_______________________________________________________________________________
#----                    sampled_distribution class                         ----
#_______________________________________________________________________________

validateSampledDistribution <- function(object) {
  return(expectZeroOrMore(object, "sampled_values"))
}

setClass(
  "sampled_distribution",
  representation(
    sampled_values = "numeric"
  ),
  contains="distribution",
  prototype=prototype(sampled_values=numeric(0)),
  validity=validateSampledDistribution
)

#_______________________________________________________________________________
#----                   constant_distribution class                         ----
#_______________________________________________________________________________

validateConstantDistribution <- function(object) {
  return(expectOneForAll(object, c("value")))
}

#' @export
setClass(
  "constant_distribution",
  representation(
    value = "numeric"
  ),
  contains="sampled_distribution",
  validity=validateConstantDistribution
)

#' 
#' Create a constant distribution. Its value will be constant across all generated samples.
#' 
#' @param value covariate value, numeric
#' @return a covariate  
#' @export
ConstantDistribution <- function(value) {
  return(new("constant_distribution", value=as.numeric(value)))
}

#_______________________________________________________________________________
#----                       fixed_distribution class                        ----
#_______________________________________________________________________________

validateFixedDistribution <- function(object) {
  return(expectOneOrMore(object, c("values")))
}

#' @export
setClass(
  "fixed_distribution",
  representation(
    values = "numeric"
  ),
  contains="sampled_distribution",
  validity=validateFixedDistribution
)

#'
#' Create a fixed distribution.
#' Each sample will be assigned a fixed value coming from vector 'values'.
#'
#' @param values covariate values, numeric vector (1 value per sample)
#' @return a covariate
#' @export
FixedDistribution <- function(values) {
  return(new("fixed_distribution", values=values))
}

#_______________________________________________________________________________
#----                   function_distribution class                         ----
#_______________________________________________________________________________

validateFunctionDistribution <- function(object) {
  check1 <- expectOne(object, "fun")
  check2 <- expectZeroOrMore(object, "args")
  return(c(check1, check2))
}

#' @export
setClass(
  "function_distribution",
  representation(
    fun = "character",
    args = "list"
  ),
  contains="sampled_distribution",
  prototype=prototype(args=list()),
  validity=validateFunctionDistribution
)

#'
#' Create a function distribution. During distribution sampling, the provided function
#' will be responsible for generating values for each sample. If first argument
#' of this function is not the size (n), please tell which argument corresponds
#' to the size 'n' (e.g. list(size="n")).
#'
#' @param fun function name, character (e.g. 'rnorm')
#' @param args list of arguments (e.g list(mean=70, sd=10))
#' @return a covariate
#' @export
FunctionDistribution <- function(fun, args) {
  return(new("function_distribution", fun=fun, args=args))
}

#_______________________________________________________________________________
#----                     bootstrap_distribution class                      ----
#_______________________________________________________________________________

validateBootstrapDistribution <- function(object) {
  check1 <- expectOneOrMore(object, c("data"))
  check2 <- expectOneForAll(object, c("replacement", "random"))
  return(c(check1, check2))
}

#' @export
setClass(
  "bootstrap_distribution",
  representation(
    data = "numeric",
    replacement = "logical",
    random = "logical"
  ),
  contains="sampled_distribution",
  prototype=prototype(replacement=FALSE, random=FALSE),
  validity=validateBootstrapDistribution
)

#'
#' Create a bootstrap distribution. During function sampling, PMXsim will generate
#' values depending on the given data and arguments.
#'
#' @param data values to draw, numeric vector
#' @param replacement values can be reused or not, logical
#' @param random values are drawn randomly, logical
#' @return a covariate
#' @export
BootstrapDistribution <- function(data, replacement=FALSE, random=FALSE) {
  return(new("bootstrap_distribution", data=data, replacement=replacement, random=random))
}

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

sample_delegate <- function(fun, n, ...) {
  return(fun(n, ...))
}

setMethod("sample", signature = c("constant_distribution", "integer"), definition = function(object, n) {
  object@sampled_values <- rep(object@value, n)
  return(object)
})

setMethod("sample", signature = c("fixed_distribution", "integer"), definition = function(object, n) {
  object@sampled_values <- object@values
  if (length(object@values) != n) {
    stop(paste0("A fixed distribution should have exactly ", n, "values, not ", length(object@values)))
  }
  return(object)
})

setMethod("sample", signature = c("function_distribution", "integer"), definition = function(object, n) {
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
  object@sampled_values <- values
  
  return(object)
})

setMethod("sample", signature = c("bootstrap_distribution", "integer"), definition = function(object, n) {
  data <- object@data
  nData <- length(data)
  replacement <- object@replacement
  random <- object@random
  
  if (n > nData && !replacement) {
    stop(paste("Provided bootstrap function does not have enough data to generate", n, "samples (please set 'replacement' to TRUE)"))
  }
  
  if (random) {
    values <- data[sample.int(n=nData, size=n, replace=replacement)]
  } else {
    values <- rep(data, length.out=n)
  }
  
  # Assign values
  object@sampled_values <- values
  return(object)
})


