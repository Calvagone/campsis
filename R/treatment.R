
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
  times <- object@list %>% purrr::map_dbl(~.x@time)
  doseNumbers <- match(times, unique(times))
  object@list <- purrr::map2(object@list, doseNumbers, .f=function(.x, .y){
    .x@dose_number <- .y
    return(.x)
  })
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

concatenateCompartments <- function(x) {
  return(paste0(x, collapse=","))
}

getAdminString <- function(object, type) {
  clz <- type$type
  cmt <- type$cmt # Concatenated version of compartment (see show method below)
  cmtSize <- type$cmtSize

  admins <- object@list %>% purrr::keep(.p=function(x){
    comp1 <- clz == (class(x) %>% as.character())
    comp2 <- cmt == concatenateCompartments(x@compartment)
    return(comp1 && comp2)
  })

  str <- paste0("-> Adm. times (", clz, " into ")
  if (cmtSize==0) {
    str <- paste0(str, "DEFAULT", "): ")
  } else {
    str <- paste0(str, "CMT=", cmt, "): ")
  }
  
  amt <- -1
  allTimes <- c()
  for (admin in admins) {
    if (admin@amount==amt) {
      allTimes <- allTimes %>% append(admin@time)
    } else {
      allTimes <- allTimes %>% append(paste0(admin@time, " (", admin@amount, ")"))
      amt <- admin@amount
    }
  }
  return(paste0(str, paste0(allTimes, collapse=",")))
}

setMethod("show", signature=c("treatment"), definition=function(object) {
  object <- object %>% sort()
  
  adminTypes <- object@list %>% purrr::map_df(.f=function(x){
    return(tibble::tibble(type=class(x) %>% as.character(),
                          cmt=concatenateCompartments(x@compartment),
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
