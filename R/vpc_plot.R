
#_______________________________________________________________________________
#----                          vpc_plot generic                             ----
#_______________________________________________________________________________

#' VPC plot (S3 generic).
#'
#' @param x a CAMPSIS output object
#' @param ... additional arguments passed to the method
#' @return a ggplot object
#' @export
vpc_plot <- function(x, ...) {
  UseMethod("vpc_plot")
}

#_______________________________________________________________________________
#----                    vpc_plot.std_campsis_tbl                           ----
#_______________________________________________________________________________

#' VPC plot for standard CAMPSIS simulation output.
#'
#' Plots prediction interval ribbons (median PI and lower/upper PI) from a
#' \code{std_campsis_tbl} produced with multiple replicates (i.e. the
#' \code{replicate} column must be present and contain more than one distinct
#' value).
#'
#' Stratification is resolved as follows:
#' \itemize{
#'   \item \code{"auto"} (default): \code{ARM} is used when more than one
#'         distinct arm is detected; otherwise \code{SCENARIO} is used when
#'         more than one distinct scenario is detected; otherwise \code{NULL}
#'         (no stratification). Only one stratification variable is applied
#'         because \code{\link{vpcPlot}} supports at most one.
#'   \item \code{NULL}: no stratification.
#'   \item named character vector: passed directly to \code{\link{vpcPlot}}
#'         (e.g. \code{c(ARM = "all")} or \code{c(SCENARIO = "all")}).
#' }
#'
#' @param x a \code{std_campsis_tbl} object with a \code{replicate} column
#' @param strata stratification for the VPC. One of \code{"auto"},
#'   \code{NULL}, or a named character vector of length 1. See Details.
#' @param level prediction interval level. Default is \code{0.90} (90\% PI).
#' @param alpha transparency of the ribbons. Default is \code{0.15}.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{vpcPlot}}
#' @importFrom ggplot2 aes ggplot geom_ribbon ylab
#' @export
vpc_plot.std_campsis_tbl <- function(x, strata = "auto",
                                     level = 0.90,
                                     alpha = 0.15,
                                     ...) {
  if (!.is_replicated(x)) {
    stop(
      "vpc_plot() requires data with multiple replicates. ",
      "Run simulate() with 'replicates > 1' and a PIOutfun() to generate VPC-ready output.",
      call. = FALSE
    )
  }

  if (identical(strata, "auto")) {
    strata <- .auto_strata(x)
  }

  vpcPlot(x, strata = strata, level = level, alpha = alpha)
}

#' Detect VPC stratification variable automatically.
#'
#' Returns a named character vector suitable for \code{\link{vpcPlot}}'s
#' \code{strata} argument (length 0 or 1), or \code{NULL} when no
#' stratification is warranted.  \code{ARM} is preferred over \code{SCENARIO}
#' when both are multi-valued.
#'
#' @param x a data frame (typically \code{std_campsis_tbl})
#' @return a named character vector or \code{NULL}
#' @keywords internal
.auto_strata <- function(x) {
  if ("ARM" %in% colnames(x) && dplyr::n_distinct(x$ARM) > 1) {
    return(c(ARM = "all"))
  }

  if ("SCENARIO" %in% colnames(x) && dplyr::n_distinct(x$SCENARIO) > 1) {
    return(c(SCENARIO = "all"))
  }

  NULL
}
