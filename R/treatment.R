
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
  times <- x@list %>% purrr::map_dbl(~.x@time[1]) # First element
  
  # Reorder
  classes <- c("bolus_wrapper", "infusion_wrapper", "bolus", "infusion")
  types <- factor(types, levels=classes, labels=classes)
  order <- order(times, types)
  
  # Apply result to original list
  x@list <- x@list[order]
  return(x)
})

#_______________________________________________________________________________
#----                        assign_dose_number                             ----
#_______________________________________________________________________________

#' Assign dose number to each treatment entry.
#' 
#' @param object treatment
#' @return updated treatment object
#' @keywords internal
assign_dose_number <- function(object) {
  stop("No default function is provided")
}

setGeneric("assign_dose_number", function(object) {
  standardGeneric("assign_dose_number")
})

setMethod("assign_dose_number", signature = c("treatment"), definition = function(object) {
  object <- object %>% sort()
  times <- object@list %>% purrr::map_dbl(~.x@time)
  doseNumbers <- match(times, unique(times))
  object@list <- purrr::map2(object@list, doseNumbers, .f=function(.x, .y){
    .x@dose_number <- .y
    return(.x)
  })
  return(object)
})

#_______________________________________________________________________________
#----                             get_times                                 ----
#_______________________________________________________________________________

#' @param unwrap unwrap treatment before accessing the times, default value is TRUE
#' @rdname get_times
setMethod("get_times", signature = c("treatment"), definition = function(object, unwrap=TRUE) {
  if (unwrap) {
    # unwrap can be FALSE on demand (see dataset export where the treatment is first unwrapped)
    object <- unwrap_treatment(object)
  }
  times <- object@list %>%
    purrr::map(~.x@time) %>%
    purrr::flatten_dbl()
  return(base::sort(unique(times)))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

getAdminString <- function(object, type) {
  clz <- type$type
  cmt <- type$cmt # Concatenated version of compartment (see show method below)
  cmtSize <- type$cmtSize

  admins <- object@list %>% purrr::keep(.p=function(x){
    comp1 <- clz == (class(x) %>% as.character())
    comp2 <- cmt == get_treatment_entry_cmt_string(x)
    return(comp1 && comp2)
  })

  str <- paste0("-> Adm. times (", clz, " into ")
  if (cmtSize==0) {
    str <- paste0(str, "DEFAULT", "): ")
  } else {
    str <- paste0(str, "CMT=", cmt, "): ")
  }
  table <- admins %>%
    purrr::map_dfr(~{
      tibble::tibble(
        TIME = .x@time,
        AMT = .x@amount
      )
    }) %>%
    dplyr::group_by(dplyr::across("TIME")) %>%
    dplyr::summarise(AMT=sum(.data$AMT)) %>%
    dplyr::mutate(AMT_STRING=ifelse(duplicated(.data$AMT), sprintf("%s", .data$TIME), sprintf("%s (%s)", .data$TIME, .data$AMT)))

  return(paste0(str, paste0(table$AMT_STRING, collapse=",")))
}

setMethod("show", signature=c("treatment"), definition=function(object) {
  
  # Unwrap treatment and sort
  object <- object %>%
    unwrap_treatment() %>%
    sort()
  
  adminTypes <- object@list %>% purrr::map_df(.f=function(x){
    return(tibble::tibble(type=class(x) %>% as.character(),
                          cmt=get_treatment_entry_cmt_string(x),
                          cmtSize=length(x@compartment)))
  }) %>% dplyr::distinct()
  
  for(index in seq_len(nrow(adminTypes))) {
    cat(getAdminString(object, adminTypes[index,] %>% as.list()))
    cat("\n")
  }
  show(object@iovs)
  show(object@occasions)
  show(object@dose_adaptations)
})

#_______________________________________________________________________________
#----                          unwrap_treatment                             ----
#_______________________________________________________________________________

#' @rdname unwrap_treatment
setMethod("unwrap_treatment", signature = c("treatment"), definition = function(object) {
  if (length(object@list)==0) {
    # Do nothing
  } else {
    object@list <- object@list %>%
      purrr::map(~unwrap_treatment(.x)) %>%
      unlist() # Return NULL if input is empty list
  }
  return(object)
})

#_______________________________________________________________________________
#----                            update_amount                              ----
#_______________________________________________________________________________

#' @rdname update_amount
setMethod("update_amount", signature = c("treatment", "numeric", "character"), definition = function(object, amount, ref) {
  object@list <- object@list %>% purrr::map(~update_amount(.x, amount, ref))
  return(object)
})

#_______________________________________________________________________________
#----                              update_ii                                ----
#_______________________________________________________________________________

#' @rdname update_ii
setMethod("update_ii", signature = c("treatment", "numeric", "character"), definition = function(object, ii, ref) {
  object@list <- object@list %>% purrr::map(~update_ii(.x, ii, ref))
  return(object)
})

#_______________________________________________________________________________
#----                             update_addl                               ----
#_______________________________________________________________________________

#' @rdname update_addl
setMethod("update_addl", signature = c("treatment", "integer", "character"), definition = function(object, addl, ref) {
  object@list <- object@list %>% purrr::map(~update_addl(.x, addl, ref))
  return(object)
})

#_______________________________________________________________________________
#----                             update_repeat                              ----
#_______________________________________________________________________________

#' @rdname update_repeat
setMethod("update_repeat", signature = c("treatment", "repeated_schedule", "character"), definition = function(object, rep, ref) {
  object@list <- object@list %>% purrr::map(~update_repeat(.x, rep, ref))
  return(object)
})


