
#_______________________________________________________________________________
#----                        treatment class                               ----
#_______________________________________________________________________________

#' 
#' Treatment class.
#' 
#' @export
setClass(
  "treatment",
  representation(
    iovs = "treatment_iovs",
    occasions = "occasions",
    dose_adaptations = "dose_adaptations"
  ),
  contains="pmx_list",
  prototype=prototype(type="treatment_entry", iovs=new("treatment_iovs"),
                      occasions=new("occasions"), dose_adaptations=new("dose_adaptations"))
)

#_______________________________________________________________________________
#----                                 add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("treatment", "treatment_iov"), definition = function(object, x) {
  object@iovs <- object@iovs %>% add(x)
  return(object)
})

setMethod("add", signature = c("treatment", "occasion"), definition = function(object, x) {
  object@occasions <- object@occasions %>% add(x)
  return(object)
})

setMethod("add", signature = c("treatment", "dose_adaptation"), definition = function(object, x) {
  object@dose_adaptations <- object@dose_adaptations %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                               delete                                  ----
#_______________________________________________________________________________

setMethod("delete", signature = c("treatment", "treatment_iov"), definition = function(object, x) {
  object@iovs <- object@iovs %>% delete(x)
  return(object)
})

setMethod("delete", signature = c("treatment", "occasion"), definition = function(object, x) {
  object@occasions <- object@occasions %>% delete(x)
  return(object)
})

setMethod("delete", signature = c("treatment", "dose_adaptation"), definition = function(object, x) {
  object@dose_adaptations <- object@dose_adaptations %>% delete(x)
  return(object)
})


#_______________________________________________________________________________
#----                                find                                   ----
#_______________________________________________________________________________

setMethod("find", signature = c("treatment", "treatment_iov"), definition = function(object, x) {
  return(object@iovs %>% find(x))
})

setMethod("find", signature = c("treatment", "occasion"), definition = function(object, x) {
  return(object@occasions %>% find(x))
})

setMethod("find", signature = c("treatment", "dose_adaptation"), definition = function(object, x) {
  return(object@dose_adaptations %>% find(x))
})

#_______________________________________________________________________________
#----                              replace                                  ----
#_______________________________________________________________________________

setMethod("replace", signature = c("treatment", "treatment_iov"), definition = function(object, x) {
  object@iovs <- object@iovs %>% replace(x)
  return(object)
})

setMethod("replace", signature = c("treatment", "occasion"), definition = function(object, x) {
  object@occasions <- object@occasions %>% replace(x)
  return(object)
})

setMethod("replace", signature = c("treatment", "dose_adaptation"), definition = function(object, x) {
  object@dose_adaptations <- object@dose_adaptations %>% replace(x)
  return(object)
})

#_______________________________________________________________________________
#----                                  sort                                 ----
#_______________________________________________________________________________

setMethod("sort", signature=c("treatment"), definition=function(x, decreasing=FALSE, ...) {
  types <- x@list %>% purrr::map_chr(~as.character(class(.x)))
  times <- x@list %>% purrr::map_dbl(~.x@time)
  
  # Reorder
  types <- factor(types, levels=c("bolus", "infusion"), labels=c("bolus", "infusion"))
  order <- order(times, types)
  
  # Apply result to original list
  x@list <- x@list[order]
  return(x)
})

#_______________________________________________________________________________
#----                       Assign dose number                              ----
#_______________________________________________________________________________

#' Assign dose number to each treatment entry.
#' 
#' @param object treatment
#' @return updated treatment object
#' @keywords internal
assignDoseNumber <- function(object) {
  stop("No default function is provided")
}

setGeneric("assignDoseNumber", function(object) {
  standardGeneric("assignDoseNumber")
})

setMethod("assignDoseNumber", signature = c("treatment"), definition = function(object) {
  object <- object %>% sort()
  times <- object@list %>% purrr::map_chr(~.x@time)
  doseNumbers <- match(times, unique(times))
  object@list <- purrr::map2(object@list, doseNumbers, .f=function(.x, .y){
    .x@dose_number <- .y
    return(.x)
  })
  return(object)
})

