
#_______________________________________________________________________________
#----                            arms class                                 ----
#_______________________________________________________________________________

#' 
#' Arms class.
#' 
#' @export
setClass(
  "arms",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="arm") 
)

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("arms", "arm"), definition = function(object, x) {
  # Auto-increment ID based on existing ID values in arms
  if (is.na(x@id)) {
    existingIds <- object@list %>% purrr::map_int(~.x@id)
    if (length(existingIds) > 0) {
      x@id <- as.integer(max(existingIds) + 1) # Increment by 1
    } else {
      x@id <- as.integer(1) # Start at 1 if no arm yet
    }
  }
  return(callNextMethod(object, x))
})


#_______________________________________________________________________________
#----                              default                                  ----
#_______________________________________________________________________________

setMethod("default", signature=c("arms"), definition=function(object, ...) {
  if (object %>% length() == 0) {
    arm = new("arm", id=as.integer(0))
    object <- object %>% add(arm)
  }
  return(object@list[[1]])
})

#_______________________________________________________________________________
#----                          getCovariateNames                            ----
#_______________________________________________________________________________

#' @rdname getCovariateNames
setMethod("getCovariateNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getCovariateNames()) %>% purrr::flatten_chr() %>% unique())
})

#_______________________________________________________________________________
#----                            getIOVNames                                ----
#_______________________________________________________________________________

#' @rdname getIOVNames
setMethod("getIOVNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getIOVNames())  %>% purrr::flatten_chr() %>% unique())
})


#_______________________________________________________________________________
#----                         getOccasionNames                              ----
#_______________________________________________________________________________

#' @rdname getOccasionNames
setMethod("getOccasionNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getOccasionNames())  %>% purrr::flatten_chr() %>% unique())
})

#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

#' @rdname getTimeVaryingCovariateNames
setMethod("getTimeVaryingCovariateNames", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getTimeVaryingCovariateNames()) %>% purrr::flatten_chr() %>% unique())
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' @rdname getTimes
setMethod("getTimes", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getTimes()) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})

