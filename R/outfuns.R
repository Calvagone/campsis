
#_______________________________________________________________________________
#----                             outfuns class                             ----
#_______________________________________________________________________________

#' 
#' Output functions class (i.e. a collection of output functions).
#' 
#' @export
setClass(
  "outfuns",
  representation(
  ),
  contains="pmx_list",
  prototype=prototype(type="outfun")
)

#'
#' Create a collection of output functions.
#'
#' @return an output_functions object
#' @export
Outfuns <- function() {
  return(new("outfuns"))
}

#' @importFrom methods callNextMethod
setMethod("add", signature=c("outfuns", "outfun"), definition=function(object, x) {
  return(methods::callNextMethod(object, x))
})

setMethod("show", signature=c("outfuns"), definition=function(object) {
  n <- object %>% length()
  if (n == 0) {
    cat("No output functions\n")
  } else {
    for (outfun in object@list) {
      cat(sprintf("Output function (name='%s')\n", outfun@name))
    }
  }
})
