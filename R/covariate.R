
#_______________________________________________________________________________
#----                        covariate class                                ----
#_______________________________________________________________________________

checkCovariate <- function(object) {
  check1 <- expectZeroOrMore(object, "sampled_values")
  check2 <- expectOne(object, "name")
  return(c(check1, check2))
}

#' @export
setClass(
  "covariate",
  representation(
    name = "character",
    sampled_values = "numeric"
  ),
  prototype=prototype(sampled_values=numeric(0)),
  validity=checkCovariate
)

#_______________________________________________________________________________
#----                     constant_covariate class                          ----
#_______________________________________________________________________________

checkConstantCovariate <- function(object) {
  return(expectOneForAll(object, c("value")))
}

#' @export
setClass(
  "constant_covariate",
  representation(
    value = "numeric"
  ),
  contains="covariate",
  validity=checkConstantCovariate
)

#' 
#' Create a constant covariate. Its value will be constant across all arms and subjects.
#' 
#' @param name covariate name, character
#' @param value covariate value, numeric
#' @return a covariate  
#' @export
ConstantCovariate <- function(name, value) {
  return(new("constant_covariate", name=name, value=value))
}

#_______________________________________________________________________________
#----                 abstract_fixed_covariate class                        ----
#_______________________________________________________________________________

checkAbstractFixedCovariate <- function(object) {
  return(TRUE)
}

setClass(
  "abstract_fixed_covariate",
  representation(
  ),
  contains="covariate",
  validity=checkAbstractFixedCovariate
)

#_______________________________________________________________________________
#----                       fixed_covariate class                           ----
#_______________________________________________________________________________

checkFixedCovariate <- function(object) {
  return(expectOneOrMore(object, c("values")))
}

#' @export
setClass(
  "fixed_covariate",
  representation(
    values = "numeric"
  ),
  contains="abstract_fixed_covariate",
  validity=checkFixedCovariate
)

#'
#' Create a fixed covariate. Each subject will be assigned a fixed value, i.e.,
#' a value that does not vary over time.
#'
#' @param name covariate name, character
#' @param values covariate values, numeric vector (1 value per subject)
#' @return a covariate
#' @export
FixedCovariate <- function(name, values) {
  return(new("fixed_covariate", name=name, values=values))
}

#_______________________________________________________________________________
#----                     function_covariate class                          ----
#_______________________________________________________________________________

checkFunctionCovariate <- function(object) {
  check1 <- expectOne(object, "fun")
  check2 <- expectZeroOrMore(object, "args")
  return(c(check1, check2))
}

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

#'
#' Create a function covariate. During covariate sampling, the provided function
#' will be responsible for generating values for each subject. If first argument
#' of this function is not the size (n), please tell which argument corresponds
#' to the size 'n' (e.g. list(size="n")).
#'
#' @param name covariate name, character
#' @param fun function name, character (e.g. 'rnorm')
#' @param args list of arguments (e.g list(mean=70, sd=10))
#' @return a covariate
#' @export
FunctionCovariate <- function(name, fun, args) {
  return(new("function_covariate", name=name, fun=fun, args=args))
}

#_______________________________________________________________________________
#----                     bootstrap_covariate class                         ----
#_______________________________________________________________________________

checkBootstrapCovariate <- function(object) {
  check1 <- expectOneOrMore(object, c("data"))
  check2 <- expectOneForAll(object, c("replacement", "random"))
  return(c(check1, check2))
}

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

#'
#' Create a bootstrap covariate. During covariate sampling, PMXsim will generate
#' values depending on the given data and arguments.
#'
#' @param name covariate name, character
#' @param data values to draw, numeric vector
#' @param replacement values can be reused or not, logical
#' @param random values are drawn randomly, logical
#' @return a covariate
#' @export
BootstrapCovariate <- function(name, data, replacement=FALSE, random=FALSE) {
  return(new("bootstrap_covariate", name=name, data=data, replacement=replacement, random=random))
}

#_______________________________________________________________________________
#----                             sample                                    ----
#_______________________________________________________________________________

#' Sample generic object.
#' 
#' @param object generic object
#' @param n number of samples required
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

setMethod("sample", signature = c("constant_covariate", "integer"), definition = function(object, n) {
  object@sampled_values <- rep(object@value, n)
  return(object)
})

setMethod("sample", signature = c("fixed_covariate", "integer"), definition = function(object, n) {
  object@sampled_values <- object@values
  if (length(object@values) != n) {
    stop(paste0("Covariate ", object@name, "should have exactly ", n, "values, not ", length(object@values)))
  }
  return(object)
})

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
  object@sampled_values <- values
  
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
  object@sampled_values <- values
  return(object)
})

