
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

#_______________________________________________________________________________
#----                                 add                                   ----
#_______________________________________________________________________________


#' @importFrom methods callNextMethod
setMethod("add", signature=c("outfuns", "outfun"), definition=function(object, x) {
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                                show                                   ----
#_______________________________________________________________________________

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

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("outfuns", "json_element"), definition=function(object, json) {
  json_outfuns <- json@data
  
  object@list <- json_outfuns %>% purrr::map(.f=function(json_outfun) {
    outfun <- loadFromJSON(object=new(json_outfun$type), json=JSONElement(json_outfun))
    return(outfun)
  })
  
  return(object)
})
