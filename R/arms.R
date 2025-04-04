
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
#----                           getCovariates                               ----
#_______________________________________________________________________________

#' @rdname getCovariates
setMethod("getCovariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% getCovariates())
})

#_______________________________________________________________________________
#----                         getEventCovariates                            ----
#_______________________________________________________________________________

#' @rdname getEventCovariates
setMethod("getEventCovariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% getEventCovariates())
})

#_______________________________________________________________________________
#----                         getFixedCovariates                            ----
#_______________________________________________________________________________

#' @rdname getFixedCovariates
setMethod("getFixedCovariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% getFixedCovariates())
})

#_______________________________________________________________________________
#----                       getTimeVaryingCovariates                        ----
#_______________________________________________________________________________

#' @rdname getTimeVaryingCovariates
setMethod("getTimeVaryingCovariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% getTimeVaryingCovariates())
})

#_______________________________________________________________________________
#----                              getIOVs                                  ----
#_______________________________________________________________________________

#' @rdname getIOVs
setMethod("getIOVs", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% getIOVs())
})

#_______________________________________________________________________________
#----                            getOccasions                               ----
#_______________________________________________________________________________

#' @rdname getOccasions
setMethod("getOccasions", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% getOccasions())
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' @rdname getTimes
setMethod("getTimes", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f=~.x %>% getTimes()) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("arms"), definition=function(object) {
  for (arm in object@list) {
    show(arm)
    cat("\n")
  }
})

#_______________________________________________________________________________
#----                          unwrapTreatment                              ----
#_______________________________________________________________________________

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature = c("arms"), definition = function(object) {
  object@list <- object@list %>% purrr::map(~.x %>% unwrapTreatment())
  return(object)
})

#_______________________________________________________________________________
#----                            updateAmount                               ----
#_______________________________________________________________________________

#' @rdname updateAmount
setMethod("updateAmount", signature = c("arms", "numeric", "character"), definition = function(object, amount, ref) {
  object@list <- object@list %>% purrr::map(~updateAmount(.x, amount, ref))
  return(object)
})

#_______________________________________________________________________________
#----                              updateII                                 ----
#_______________________________________________________________________________

#' @rdname updateII
setMethod("updateII", signature = c("arms", "numeric", "character"), definition = function(object, ii, ref) {
  object@list <- object@list %>% purrr::map(~updateII(.x, ii, ref))
  return(object)
})

#_______________________________________________________________________________
#----                             updateADDL                                ----
#_______________________________________________________________________________

#' @rdname updateADDL
setMethod("updateADDL", signature = c("arms", "integer", "character"), definition = function(object, addl, ref) {
  object@list <- object@list %>% purrr::map(~updateADDL(.x, addl, ref))
  return(object)
})

#_______________________________________________________________________________
#----                             updateRepeat                              ----
#_______________________________________________________________________________

#' @rdname updateRepeat
setMethod("updateRepeat", signature = c("arms", "repeated_schedule", "character"), definition = function(object, rep, ref) {
  object@list <- object@list %>% purrr::map(~updateRepeat(.x, rep, ref))
  return(object)
})


