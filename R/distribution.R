#_______________________________________________________________________________
#----                        distribution class                             ----
#_______________________________________________________________________________

validateDistribution <- function(object) {
  return(TRUE)
}

#' 
#' Distribution class. See this class as an interface.
#' 
#' @export
setClass(
  "distribution",
  representation(
  ),
  contains="pmx_element",
  validity=validateDistribution
)

#_______________________________________________________________________________
#----                     undefined_distribution class                      ----
#_______________________________________________________________________________

#' 
#' Undefined distribution class. This type of object is automatically created
#' in method toExplicitDistribution() when the user does not provide a concrete
#' distribution. This is because S4 objects do not accept NULL values.
#' 
#' @export
setClass(
  "undefined_distribution",
  representation(
  ),
  contains="distribution"
)

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

#' 
#' Constant distribution class.
#' 
#' @slot value covariate value, single numeric value
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
#' @param value covariate value, single numeric value
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

#' 
#' Fixed distribution class.
#' 
#' @slot values covariate values, numeric vector (1 value per sample)
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

#' 
#' Function distribution class.
#' 
#' @slot fun function name, character (e.g. 'rnorm')
#' @slot args list of arguments (e.g list(mean=70, sd=10))
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
#' @return a function distribution
#' @export
FunctionDistribution <- function(fun, args) {
  return(new("function_distribution", fun=fun, args=args))
}

#'
#' Create an uniform distribution.
#'
#' @param min min value
#' @param max max value
#' @return an uniform distribution
#' @export
UniformDistribution <- function(min, max) {
  expectSingleNumericValue(min, "min")
  expectSingleNumericValue(max, "max")
  return(new("function_distribution", fun="runif", args=list(min=as.numeric(min), max=as.numeric(max))))
}

#'
#' Create a normal distribution.
#'
#' @param mean mean
#' @param sd sd
#' @return a normal distribution
#' @export
NormalDistribution <- function(mean, sd) {
  expectSingleNumericValue(mean, "mean")
  expectSingleNumericValue(sd, "sd")
  return(new("function_distribution", fun="rnorm", args=list(mean=as.numeric(mean), sd=as.numeric(sd))))
}

#'
#' Create a log normal distribution.
#'
#' @param meanlog mean in log domain
#' @param sdlog sd in log domain
#' @return a log normal distribution
#' @export
LogNormalDistribution <- function(meanlog, sdlog) {
  expectSingleNumericValue(meanlog, "meanlog")
  expectSingleNumericValue(sdlog, "sdlog")
  return(new("function_distribution", fun="rlnorm", args=list(meanlog=as.numeric(meanlog), sdlog=as.numeric(sdlog))))
}

#'
#' Discrete distribution.
#'
#' @param x vector of one or more integers from which to choose
#' @param prob a vector of probability weights for obtaining the elements of the vector being sampled
#' @param replace should sampling be with replacement, default is TRUE
#' @return a discrete distribution
#' @export
DiscreteDistribution <- function(x, prob, replace=TRUE) {
  assertthat::assert_that(is.numeric(x), msg="x must be numeric")
  assertthat::assert_that(is.numeric(prob), msg="prob must be numeric")
  assertthat::assert_that(length(x)==length(prob), msg="x and prob must have the same length")
  return(new("function_distribution", fun="base::sample", args=list(size="n", x=as.numeric(x), prob=as.numeric(prob), replace=as.logical(replace))))
}

#' 
#' Retrieve the parameter value (standardised) for the specified parameter name.
#' 
#' @param model model
#' @param paramName parameter name
#' @param default defaut value if not found
#' @param mandatory must be in model or not
#' @return the parameter value (or a defautl value if parameter is not found)
#' @importFrom assertthat assert_that
#' @export
retrieveParameterValue <- function(model, paramName, default=NULL, mandatory=FALSE) {
  assertthat::assert_that(is.character(paramName) && length(paramName)==1,
                          msg="paramName must be a single character value")
  parameter <- model@parameters %>% getByName(paramName)
  
  if (parameter %>% length() == 0) {
    if (is.null(default) && mandatory) {
      stop(paste0(paramName, " can't be found in model"))
    }
    return(default)
  } else {
    # If parameter is OMEGA, it needs to be standardised before taking its value
    # This way, value is always a variance (or covariance)
    parameter <- parameter %>% standardise()
    return(parameter@value)
  }
}

#' 
#' Create a parameter distribution. The resulting distribution is a
#' log-normal distribution, with meanlog=log(THETA) and sdlog=sqrt(OMEGA).
#' 
#' @param model model
#' @param theta corresponding THETA name, character
#' @param omega corresponding OMEGA name, character, NULL if not defined
#' @return a parameter distribution  
#' @export
ParameterDistribution <- function(model, theta, omega=NULL) {
  thetaValue <- retrieveParameterValue(model, paramName=paste0("THETA_", theta), mandatory=TRUE)
  if (is.null(omega)) {
    omegaValue <- 0
  } else {
    omegaValue <- retrieveParameterValue(model, paramName=paste0("OMEGA_", omega), mandatory=TRUE)
  }
  fun <- FunctionDistribution(fun="rlnorm", args=list(meanlog=log(thetaValue), sdlog=sqrt(omegaValue)))
  return(fun)
}

#' 
#' Create an ETA distribution. The resulting distribution is a
#' normal distribution, with mean=0 and sd=sqrt(OMEGA)
#' 
#' @param model model
#' @param omega corresponding THETA name, character
#' @return an ETA distribution
#' @export
EtaDistribution <- function(model, omega) {
  omegaValue <- retrieveParameterValue(model, paramName=paste0("OMEGA_", omega), mandatory=TRUE)
  return(NormalDistribution(mean=0, sd=sqrt(omegaValue)))
}

#_______________________________________________________________________________
#----                     bootstrap_distribution class                      ----
#_______________________________________________________________________________

validateBootstrapDistribution <- function(object) {
  check1 <- expectOneOrMore(object, c("data"))
  check2 <- expectOneForAll(object, c("replacement", "random"))
  return(c(check1, check2))
}

#' 
#' Bootstrap distribution class.
#' 
#' @slot data values to draw, numeric vector
#' @slot replacement values can be reused or not, logical
#' @slot random values are drawn randomly, logical
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

sample_delegate <- function(fun, ...) {
  return(fun(...))
}

sample_delegate_n <- function(fun, n, ...) {
  return(fun(n, ...))
}

#' @rdname sample
setMethod("sample", signature = c("constant_distribution", "integer"), definition = function(object, n) {
  object@sampled_values <- rep(object@value, n)
  return(object)
})

#' @rdname sample
setMethod("sample", signature = c("fixed_distribution", "integer"), definition = function(object, n) {
  object@sampled_values <- object@values
  if (length(object@values) != n) {
    stop(paste0("A fixed distribution should have exactly ", n, " values, not ", length(object@values)))
  }
  return(object)
})

#' @rdname sample
setMethod("sample", signature = c("function_distribution", "integer"), definition = function(object, n) {
  fun <- eval(parse(text=object@fun))
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
      if (.z[[1]]=="n") {
        .z <- "SAMPLING_SIZE"
      }
      if (length(.z) > 1) {
        value <- paste0("c(", paste0(.z, collapse=","), ")")
      } else {
        value <- .z
      }
      paste0(.x, comma, .y , "=", value)
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
  if (hasNCharAsValue) {
    text <- paste0("sample_delegate(fun, ", args_str, ")")
  } else {
    text <- paste0("sample_delegate_n(fun, ", args_str, ")")
  }
  
  values <- eval(parse(text=text))
  
  # Check result
  if (length(values) != n) {
    stop(paste("Calling function", object@fun, "with arguments", args_str, "does not provided", n, "values but", length(values)))
  }
  
  # Assign values
  object@sampled_values <- values
  
  return(object)
})

#' @rdname sample
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


