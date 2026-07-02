#' Standard Campsis table class
#'
#' @name std_campsis_tbl
#' @exportClass std_campsis_tbl
setOldClass(c("std_campsis_tbl", "campsis_tbl", "tbl_df", "data.frame"))

#' Campsis table class (see this class as an interface)
#'
#' @name campsis_tbl
#' @exportClass campsis_tbl
setOldClass(c("campsis_tbl", "tbl_df", "data.frame"))
