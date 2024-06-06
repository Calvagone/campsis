#_______________________________________________________________________________
#----                         output_function class                         ----
#_______________________________________________________________________________

#' 
#' Output function class.
#' 
#' @slot fun function or purrr-style lambda formula, first argument 'x' must be the results
#' @slot args extra arguments, named list
#' @slot function arguments
#' @slot level either 'scenario' or 'replicate'. Default is 'scenario'.
#' @export
setClass(
  "output_function",
  representation(
    fun="function",
    args="list",
    level="character"
  ),
  prototype=prototype(fun=function(x, ...){x}, level="scenario")
)

#'
#' Create a new output function
#'
#' @param fun function or purrr-style lambda formula, first argument 'x' must be the results
#' @param args extra arguments, named list
#' @param level either 'scenario' or 'replicate'. Default is 'scenario'.
#' @importFrom rlang as_function is_formula
#' @return an output function
#' @export
Outfun <- function(fun=function(x, ...){x}, args=list(), level="scenario") {
  if (is.function(fun)) {
    # Do nothing
  } else if (rlang::is_formula(fun)) {
    fun <- rlang::as_function(fun)
    class(fun) <- "function" # Cast needed to work with S4 class system
  } else {
    stop("fun must be a function or a purrr-style lambda formula") 
  }
  assertthat::assert_that(level %in% c("scenario", "replicate"), msg="Level must be 'scenario' or 'replicate'")
    
  return(new("output_function", fun=fun, args=args, level=level))
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
    x <- do.call(outfun@fun, args=args)
  }
  return(x)
}
