
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
    iovs = "treatment_iovs"
  ),
  contains="pmx_list",
  prototype=prototype(type="treatment_entry", iovs=new("treatment_iovs"))
)

#_______________________________________________________________________________
#----                                 add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("treatment", "treatment_iov"), definition = function(object, x) {
  object@iovs <- object@iovs %>% add(x)
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
#' @export
assignDoseNumber <- function(object) {
  stop("No default function is provided")
}

setGeneric("assignDoseNumber", function(object) {
  standardGeneric("assignDoseNumber")
})

setMethod("assignDoseNumber", signature = c("treatment"), definition = function(object) {
  object <- object %>% pmxmod::sort()
  times <- object@list %>% purrr::map_chr(~.x@time)
  doseNumbers <- match(times, unique(times))
  object@list <- purrr::map2(object@list, doseNumbers, .f=function(.x, .y){
    .x@dose_number <- .y
    return(.x)
  })
  return(object)
})

