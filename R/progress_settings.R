#_______________________________________________________________________________
#----                         progress_settings class                       ----
#_______________________________________________________________________________

#' 
#' Progress settings class.
#' 
#' @slot tick_slice tick() is called after each simulated slice, default is TRUE.
#' In some cases, when the number of subjects per slice is low, it may be useful disable this flag,
#' to improve performance issues.
#' @export
setClass(
  "progress_settings",
  representation(
    tick_slice="logical"
  ),
  prototype=prototype(tick_slice=TRUE)
)

#'
#' Create progress settings.
#'
#' @param tick_slice tick() is called after each simulated slice, default is TRUE.
#' In some cases, when the number of subjects per slice is low, it may be useful disable this flag,
#' to improve performance issues.
#'
#' @return progress settings
#' @export
Progress <- function(tick_slice=TRUE) {
  return(new("progress_settings", tick_slice=tick_slice))
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("progress_settings"), definition=function(object) {
  if (identical(object, Progress())) {
    cat("")  
  } else {
    cat(sprintf("Progress: tick_slice=%s", as.character(object@tick_slice)))
    cat("\n")
  }
})
