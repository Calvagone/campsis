
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
#----                            apply_outfun                               ----
#_______________________________________________________________________________

#' @importFrom stats setNames
#' @rdname apply_outfun
setMethod("apply_outfun", signature = c(outfun = "outfuns"), definition = function(x, outfun, level, ...) {
  hasReplicate <- "replicate" %in% colnames(x)

  # Apply a single output function, per replicate when a 'replicate' column is present
  applyOne <- function(fun) {
    if (hasReplicate) {
      x %>%
        dplyr::group_by(dplyr::across(dplyr::all_of("replicate"))) %>%
        dplyr::group_split() %>%
        purrr::map_dfr(function(sub) {
          replicate_i <- sub$replicate[1]
          apply_outfun(
            x = sub %>% dplyr::select(-dplyr::all_of("replicate")),
            outfun = fun, level = level, replicate = replicate_i, ...
          ) %>%
            dplyr::mutate(replicate = replicate_i) %>%
            dplyr::relocate(dplyr::all_of("replicate"))
        })
    } else {
      apply_outfun(x = x, outfun = fun, level = level, ...)
    }
  }

  # Return a named list of results (one entry per output function)
  outfun@list %>%
    purrr::map(applyOne) %>%
    stats::setNames(outfun@list %>% purrr::map_chr(~ .x@name))
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
