#_______________________________________________________________________________
#----                         output_function class                         ----
#_______________________________________________________________________________

#' 
#' Output function class.
#' 
#' @slot fun function or purrr-style lambda formula
#' @slot level either 'scenario' or 'replicate'. Default is 'scenario'.
#' @export
setClass(
  "output_function",
  representation(
    fun="function",
    level="character"
  ),
  prototype=prototype(fun=function(x, ...){x}, level="scenario")
)

#'
#' Create a new output function
#'
#' @param fun function or purrr-style lambda formula
#' @param level either 'scenario' or 'replicate'. Default is 'scenario'.
#' @importFrom rlang as_function is_formula
#' @return an output function
#' @export
Outfun <- function(fun=function(x, ...){x}, level="scenario") {
  if (is.function(fun)) {
    # Do nothing
  } else if (rlang::is_formula(fun)) {
    fun <- rlang::as_function(fun)
    class(fun) <- "function" # Cast needed to work with S4 class system
  } else {
    stop("fun must be a function or a purrr-style lambda formula") 
  }
  assertthat::assert_that(level %in% c("scenario", "replicate"), msg="Level must be 'scenario' or 'replicate'")
    
  return(new("output_function", fun=fun, level=level))
}

applyOutfun <- function(x, outfun, level, ...) {
  assertthat::assert_that(is(outfun, "output_function"), msg="x is not an output function")
  
  if (level==outfun@level) {
    x <- outfun@fun(x)
  }
  return(x)
}