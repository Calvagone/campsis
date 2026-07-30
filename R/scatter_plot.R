#_______________________________________________________________________________
#----                         scatter_plot generic                          ----
#_______________________________________________________________________________

#' Scatter plot (S3 generic).
#'
#' @param x a CAMPSIS output object
#' @param ... additional arguments passed to the method
#' @return a ggplot object
#' @export
scatter_plot <- function(x, ...) {
  UseMethod("scatter_plot")
}

#_______________________________________________________________________________
#----                   scatter_plot.std_campsis_tbl                        ----
#_______________________________________________________________________________

#' Scatter plot for standard CAMPSIS simulation output.
#'
#' Plots one or two variables against each other at a given time point, from a
#' \code{std_campsis_tbl} (the default output of \code{simulate()} when no
#' custom \code{outfun} is used).
#'
#' When a single variable is supplied it is plotted on the x-axis with
#' y fixed to 0, producing a 1-D strip chart. When two variables are supplied
#' the first is mapped to x and the second to y.
#'
#' Colour stratification is resolved as follows:
#' \itemize{
#'   \item \code{"auto"} (default): \code{ARM} is added when more than one
#'         distinct arm is detected in the data; \code{SCENARIO} is added when
#'         more than one distinct scenario is detected.
#'   \item \code{NULL}: no colour mapping.
#'   \item character vector: explicit column name(s) to colour by (forwarded
#'         directly to \code{\link{scatterPlot}}).
#' }
#'
#' @param x a \code{std_campsis_tbl} object
#' @param variable character vector of length 1 or 2 giving the column name(s)
#'   to plot. Defaults to \code{"CONC"}. An informative error is raised when
#'   any column is absent.
#' @param colour stratification for point colour. One of \code{"auto"},
#'   \code{NULL}, or a character vector of column names. See Details.
#' @param time numeric vector of time point(s) to filter to before plotting.
#'   When \code{NULL} (default) the minimum time in the data is used (typically
#'   time 0), which is useful for plotting individual parameters. Forwarded to
#'   \code{\link{scatterPlot}}.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{scatterPlot}}
#' @importFrom ggplot2 aes ggplot geom_point labs
#' @export
scatter_plot.std_campsis_tbl <- function(x, variable = "CONC", colour = "auto", time = NULL, ...) {
  if (length(variable) > 2) {
    stop("'variable' must have length 1 or 2.", call. = FALSE)
  }

  .assert_variable_present(x, variable)

  if (identical(colour, "auto")) {
    colour <- .auto_colour_columns(x)
  }

  scatterPlot(x, variable = variable, colour = colour, time = time)
}
