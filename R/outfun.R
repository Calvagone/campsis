#_______________________________________________________________________________
#----                              outfun class                             ----
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
  "outfun",
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

setMethod("getName", signature=c("outfun"), definition=function(x) {
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
   
  return(new("outfun", fun=fun, fun_name=fun_name, args=args, packages=packages, level=level))
}

applyOutfun <- function(x, outfun, level, ...) {
  assertthat::assert_that(is(outfun, "outfun"), msg="x is not an output function")
  
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
#----                            pi_outfun class                            ----
#_______________________________________________________________________________

#'
#' Prediction interval output function class.
#'
#' @slot variable variable(s) used to compute the prediction interval, character vector
#' @slot strata named vector with the strata to use
#' @slot level PI level, default is 0.9 (90\% PI)
#' @export
setClass(
  "pi_outfun",
  representation(
    variable="character",
    strata="vector",
    pi_level="numeric"  
  ),
  contains="outfun",
  prototype=prototype(
    variable=character(0),
    strata=getDefaultStrata(),
    pi_level=0.90,
    fun=function(x, ...) { x },
    level="replicate",
    fun_name="default_pi"
  )
)

#'
#' Create a prediction interval output function
#'
#' @param variable variable(s) used to compute the prediction interval, character vector
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all")
#' @param level PI level, default is 0.9 (90\% PI)
#' @param fun_name name of the output function. Default is 'pi_<variable>_<level>pc'.
#' @importFrom assertthat assert_that
#' @return a pi_outfun object
#' @export
PIOutfun <- function(variable, strata = getDefaultStrata(), level = 0.9,
  fun_name = sprintf("PI_%s_%i%%", paste0(variable, collapse="_"), round(level*100))) {

  assertthat::assert_that(
    is.character(variable) && length(variable) >= 1,
    msg = "variable must be a non-empty character vector"
  )
  assertthat::assert_that(
    is.null(strata) || (is.atomic(strata) && !is.null(names(strata)) && all(nzchar(names(strata)))),
    msg = "strata must be a fully named vector or NULL"
  )
  assertthat::assert_that(
    is.numeric(level) && level > 0 && level < 1,
    msg = "level must be a numeric value between 0 and 1"
  )
  
  # Create the wrapper function that delegates to PI
  pi_wrapper <- function(x, ...) {
    PI(x = x, variable = variable, strata = strata, level = level)
  }
  
  return(new(
    "pi_outfun",
    fun = pi_wrapper,
    fun_name = fun_name,
    args = list(),
    packages = character(0),
    level = "replicate",
    variable = variable,
    strata = strata,
    pi_level = level
  ))
}
