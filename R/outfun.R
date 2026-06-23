#_______________________________________________________________________________
#----                         output_function class                         ----
#_______________________________________________________________________________

#' 
#' Output function class.
#' 
#' @slot fun function or purrr-style lambda formula, first argument 'x' must be the results
#' @slot args extra arguments, named list
#' @slot packages packages that must be loaded to execute the given function, character vector
#' @slot level either 'scenario' or 'replicate'. Default is 'scenario'.
#' @export
setClass(
  "output_function",
  representation(
    fun="function",
    fun_name="character",
    args="list",
    packages="character",
    level="character"
  ),
  contains="pmx_element",
  prototype=prototype(fun=function(x, ...){x}, level="scenario", fun_name="default")
)

setMethod("getName", signature=c("output_function"), definition=function(x) {
  return(x@fun_name)
})

#'
#' Create a new output function
#'
#' @param fun function or purrr-style lambda formula, first argument 'x' must be the results
#' @param args extra arguments, named list
#' @param packages packages that must be loaded to execute the given function, character vector
#' @param level where to apply the output function, only 'replicate' is allowed since Campsis v1.9.0
#' @param fun_name name of the output function. Default is 'default'.
#' @importFrom rlang as_function is_formula
#' @return an output function
#' @export
Outfun <- function(fun=function(x, ...){x}, args=list(), packages=NULL, level="replicate", fun_name="default") {
  if (is.function(fun)) {
    # Do nothing
  } else if (rlang::is_formula(fun)) {
    fun <- rlang::as_function(fun)
    class(fun) <- "function" # Cast needed to work with S4 class system
  } else {
    stop("fun must be a function or a purrr-style lambda formula") 
  }
  assertthat::assert_that(level %in% c("replicate"), msg="No level other than 'replicate' is allowed since Campsis v1.9.0")
  if (is.null(packages)) {
    packages <- character(0)
  } 
   
  return(new("output_function", fun=fun, fun_name=fun_name, args=args, packages=packages, level=level))
}

applyOutfun <- function(x, outfun, level, ...) {
  assertthat::assert_that(is(outfun, "output_function"), msg="x is not an output function")
  
  if (level==outfun@level) {
    # Retrieve all formal arguments of the user-given function
    formalArgs_ <- formalArgs(outfun@fun)
    
    # Prepare list of arguments
    args <- list(x) %>% # First argument is the Campsis results
      append(outfun@args) # user-given arguments list
    
    # Some more arguments (like 'replicate' or 'scenario') are transmitted by Campsis automatically
    # This requires the three dot ellipsis to be there
    # Note that if lambda was passed in 'Outfun' constructor, three dot ellipsis is always there
    if ("..." %in% formalArgs_) {
      args <- args %>%
        append(list(...))
    }
    
    # Load packages
    lapply(outfun@packages, require, character.only=TRUE)
    
    # Call output function with args
    x <- do.call(outfun@fun, args=args)
  }
  return(x)
}

#_______________________________________________________________________________
#----                        output_functions class                         ----
#_______________________________________________________________________________

#' 
#' Output functions class.
#' 
#' @export
setClass(
  "output_functions",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="output_function")
)

#'
#' Create a collection of output functions.
#'
#' @return an output_functions object
#' @export
Outfuns <- function() {
  return(new("output_functions"))
}

#' @importFrom methods callNextMethod
setMethod("add", signature=c("output_functions", "output_function"), definition=function(object, x) {
  return(methods::callNextMethod(object, x))
})

setMethod("show", signature=c("output_functions"), definition=function(object) {
  n <- object %>% length()
  if (n == 0) {
    cat("No output functions\n")
  } else {
    for (outfun in object@list) {
      cat(sprintf("Output function (name='%s')\n", outfun@fun_name))
    }
  }
})
