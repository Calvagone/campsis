#' Standard Campsis table class
#'
#' @name std_campsis_tbl-class
#' @aliases std_campsis_tbl
#' @docType class
#' @exportClass std_campsis_tbl
setOldClass(c("std_campsis_tbl", "campsis_tbl", "tbl_df", "data.frame"))

#' Campsis table class (see this class as an interface)
#'
#' @name campsis_tbl-class
#' @aliases campsis_tbl
#' @docType class
#' @exportClass campsis_tbl
setOldClass(c("campsis_tbl", "tbl_df", "data.frame"))