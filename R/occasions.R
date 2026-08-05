#_______________________________________________________________________________
#----                           occasions class                             ----
#_______________________________________________________________________________

#'
#' Occasions class.
#'
#' @export
setClass(
  "occasions",
  representation(),
  contains = "pmx_list",
  prototype = prototype(type = "occasion")
)

#_______________________________________________________________________________
#----                            get_names                                   ----
#_______________________________________________________________________________

setMethod("get_names", signature = c("occasions"), definition = function(object) {
  return(object@list %>% purrr::map_chr(.f = ~ .x@colname))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("occasions"), definition = function(object) {
  if (object %>% length() > 0) {
    cat("-> Treatment occasions:", paste0(object %>% get_names(), collapse = ","))
    cat("\n")
  }
})
