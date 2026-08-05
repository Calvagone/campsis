#' Campsis table class (see this class as an interface)
#'
#' @name campsis_tbl-class
#' @aliases campsis_tbl
#' @docType class
#' @exportClass campsis_tbl
setOldClass(c("campsis_tbl", "tbl_df", "data.frame"))

#' Standard Campsis table class
#'
#' @name std_campsis_tbl-class
#' @aliases std_campsis_tbl
#' @docType class
#' @exportClass std_campsis_tbl
setOldClass(c("std_campsis_tbl", "campsis_tbl", "tbl_df", "data.frame"))

#' Prediction Interval (PI) Campsis table class
#'
#' @name pi_campsis_tbl-class
#' @aliases pi_campsis_tbl
#' @docType class
#' @exportClass pi_campsis_tbl
setOldClass(c("pi_campsis_tbl", "campsis_tbl", "tbl_df", "data.frame"))

#' Statistics Campsis table class
#'
#' @name stats_campsis_tbl-class
#' @aliases stats_campsis_tbl
#' @docType class
#' @exportClass stats_campsis_tbl
setOldClass(c("stats_campsis_tbl", "campsis_tbl", "tbl_df", "data.frame"))
