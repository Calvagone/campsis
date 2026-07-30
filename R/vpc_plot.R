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
#----                     vpc_plot.pi_campsis_tbl                           ----
#_______________________________________________________________________________

#' VPC plot for prediction-interval CAMPSIS output.
#'
#' Plots VPC ribbons (a confidence interval around the median, lower and upper
#' percentiles) from a \code{pi_campsis_tbl}, i.e. the output of
#' \code{simulate()} with multiple replicates and a \code{\link{PIOutfun}}.
#' Such data already contains the per-replicate prediction interval in long
#' format (columns \code{replicate}, \code{TIME}, \code{metric} with values
#' \code{"low"} / \code{"med"} / \code{"up"} and \code{value}). This method
#' only needs to compute the confidence interval around each percentile across
#' replicates.
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
#' @param x a \code{pi_campsis_tbl} object with a \code{replicate} column
#' @param strata stratification for the VPC. One of \code{"auto"},
#'   \code{NULL}, or a named character vector of length 1. See Details.
#' @param level confidence interval level for the ribbons around each
#'   percentile. Default is \code{0.90} (90\% CI).
#' @param alpha transparency of the ribbons. Default is \code{0.15}.
#' @param facet how the stratification variable is displayed when present.
#'   \code{TRUE} (default) draws one panel per stratum; \code{FALSE} overlays
#'   the strata in a single panel and maps the stratum to the ribbon fill
#'   colour. Forwarded to \code{\link{vpcPlot}}.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{vpcPlot}}, \code{\link{PIOutfun}}
#' @export
vpc_plot.pi_campsis_tbl <- function(x, strata = "auto", level = 0.90, alpha = 0.15, facet = TRUE, ...) {
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

  vpcPlot(x, strata = strata, level = level, alpha = alpha, facet = facet)
}

#_______________________________________________________________________________
#----                    vpc_plot.std_campsis_tbl                           ----
#_______________________________________________________________________________

#' VPC plot for standard CAMPSIS simulation output.
#'
#' Plots VPC ribbons (a confidence interval around the median, lower and upper
#' percentiles) from a \code{std_campsis_tbl}, i.e. the default output of
#' \code{simulate()} with multiple replicates and no custom \code{outfun}
#' (\code{\link{DefaultOutfun}}). In this case the data contains all
#' unsummarised individual profiles, so the per-replicate prediction interval
#' is computed first (the lower/median/upper percentiles of \code{variable}
#' across individuals within each replicate), before the confidence interval
#' around each percentile is computed across replicates.
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
#' @param variable name of the column to summarise on the y-axis. Defaults to
#'   \code{"CONC"}. An informative error is raised when the column is absent.
#' @param strata stratification for the VPC. One of \code{"auto"},
#'   \code{NULL}, or a named character vector of length 1. See Details.
#' @param pi_level prediction interval level, i.e. the percentiles of the data
#'   computed within each replicate. Default is \code{0.90} (90\% PI, i.e. the
#'   5th, 50th and 95th percentiles).
#' @param ci_level confidence interval level for the ribbons around each
#'   percentile across replicates. Default is \code{0.90} (90\% CI).
#' @param alpha transparency of the ribbons. Default is \code{0.15}.
#' @param facet how the stratification variable is displayed when present.
#'   \code{TRUE} (default) draws one panel per stratum; \code{FALSE} overlays
#'   the strata in a single panel and maps the stratum to the ribbon fill
#'   colour. Forwarded to \code{\link{vpcPlot}}.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{vpcPlot}}, \code{\link{vpc_plot.pi_campsis_tbl}}
#' @export
vpc_plot.std_campsis_tbl <- function(
  x,
  variable = "CONC",
  strata = "auto",
  pi_level = 0.90,
  ci_level = 0.90,
  alpha = 0.15,
  facet = TRUE,
  ...
) {
  if (!.is_replicated(x)) {
    stop(
      "vpc_plot() requires data with multiple replicates. ",
      "Run simulate() with 'replicates > 1' to generate VPC-ready output.",
      call. = FALSE
    )
  }

  .assert_variable_present(x, variable)

  if (identical(strata, "auto")) {
    strata <- .auto_strata(x)
  }

  # Compute the per-replicate prediction interval (lower/median/upper
  # percentiles of 'variable' across individuals, within each replicate and
  # stratum). This produces the long 'metric'/'value' format expected by
  # vpcPlot(), matching the output of a PIOutfun().
  pi <- compute_pi(
    x = x,
    variable = variable,
    strata = c(replicate = all_strata_levels(), strata),
    level = pi_level
  )

  vpcPlot(pi, strata = strata, level = ci_level, alpha = alpha, facet = facet)
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
