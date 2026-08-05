#_______________________________________________________________________________
#----                            arms class                                 ----
#_______________________________________________________________________________

#'
#' Arms class.
#'
#' @export
setClass(
  "arms",
  representation(),
  contains = "pmx_list",
  prototype = prototype(type = "arm")
)

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("arms", "arm"), definition = function(object, x) {
  # Auto-increment ID based on existing ID values in arms
  if (is.na(x@id)) {
    existingIds <- object@list %>% purrr::map_int(~ .x@id)
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

setMethod("default", signature = c("arms"), definition = function(object, ...) {
  if (object %>% length() == 0) {
    arm <- new("arm", id = as.integer(0))
    object <- object %>% add(arm)
  }
  return(object@list[[1]])
})

#_______________________________________________________________________________
#----                           get_covariates                              ----
#_______________________________________________________________________________

#' @rdname get_covariates
setMethod("get_covariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% get_covariates())
})

#_______________________________________________________________________________
#----                        get_event_covariates                            ----
#_______________________________________________________________________________

#' @rdname get_event_covariates
setMethod("get_event_covariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% get_event_covariates())
})

#_______________________________________________________________________________
#----                        get_fixed_covariates                           ----
#_______________________________________________________________________________

#' @rdname get_fixed_covariates
setMethod("get_fixed_covariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% get_fixed_covariates())
})

#_______________________________________________________________________________
#----                     get_time_varying_covariates                       ----
#_______________________________________________________________________________

#' @rdname get_time_varying_covariates
setMethod("get_time_varying_covariates", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% get_time_varying_covariates())
})

#_______________________________________________________________________________
#----                             get_iovs                                  ----
#_______________________________________________________________________________

#' @rdname get_iovs
setMethod("get_iovs", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% get_iovs())
})

#_______________________________________________________________________________
#----                            get_occasions                              ----
#_______________________________________________________________________________

#' @rdname get_occasions
setMethod("get_occasions", signature = c("arms"), definition = function(object) {
  return(object %>% default() %>% get_occasions())
})

#_______________________________________________________________________________
#----                             get_times                                 ----
#_______________________________________________________________________________

#' @rdname get_times
setMethod("get_times", signature = c("arms"), definition = function(object) {
  return(object@list %>% purrr::map(.f = ~ .x %>% get_times()) %>% purrr::flatten_dbl() %>% unique() %>% base::sort())
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("arms"), definition = function(object) {
  for (arm in object@list) {
    show(arm)
    cat("\n")
  }
})

#_______________________________________________________________________________
#----                          unwrap_treatment                             ----
#_______________________________________________________________________________

#' @rdname unwrap_treatment
setMethod("unwrap_treatment", signature = c("arms"), definition = function(object) {
  object@list <- object@list %>% purrr::map(~ .x %>% unwrap_treatment())
  return(object)
})

#_______________________________________________________________________________
#----                            update_amount                              ----
#_______________________________________________________________________________

#' @rdname update_amount
setMethod("update_amount", signature = c("arms", "numeric", "character"), definition = function(object, amount, ref) {
  object@list <- object@list %>% purrr::map(~ update_amount(.x, amount, ref))
  return(object)
})

#_______________________________________________________________________________
#----                              update_ii                                ----
#_______________________________________________________________________________

#' @rdname update_ii
setMethod("update_ii", signature = c("arms", "numeric", "character"), definition = function(object, ii, ref) {
  object@list <- object@list %>% purrr::map(~ update_ii(.x, ii, ref))
  return(object)
})

#_______________________________________________________________________________
#----                             update_addl                               ----
#_______________________________________________________________________________

#' @rdname update_addl
setMethod("update_addl", signature = c("arms", "integer", "character"), definition = function(object, addl, ref) {
  object@list <- object@list %>% purrr::map(~ update_addl(.x, addl, ref))
  return(object)
})

#_______________________________________________________________________________
#----                             update_repeat                              ----
#_______________________________________________________________________________

#' @rdname update_repeat
setMethod(
  "update_repeat",
  signature = c("arms", "repeated_schedule", "character"),
  definition = function(object, rep, ref) {
    object@list <- object@list %>% purrr::map(~ update_repeat(.x, rep, ref))
    return(object)
  }
)
