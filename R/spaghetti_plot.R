#_______________________________________________________________________________
#----                        spaghetti_plot generic                         ----
#_______________________________________________________________________________

#' Spaghetti plot (S3 generic).
#'
#' @param x a Campsis output object
#' @param ... additional arguments passed to the method
#' @return a ggplot object
#' @export
spaghetti_plot <- function(x, ...) {
  UseMethod("spaghetti_plot")
}

#_______________________________________________________________________________
#----                  spaghetti_plot.std_campsis_tbl                       ----
#_______________________________________________________________________________

#' Spaghetti plot for standard Campsis simulation output.
#'
#' Plots individual-level time profiles from a \code{std_campsis_tbl} (the
#' default output of \code{simulate()} when no custom \code{outfun} is used).
#'
#' Colour stratification is resolved as follows:
#' \itemize{
#'   \item \code{"auto"} (default): \code{ARM} is added when more than one
#'         distinct arm is detected in the data; \code{SCENARIO} is added when
#'         more than one distinct scenario is detected.
#'   \item \code{NULL}: no colour mapping.
#'   \item character vector: explicit column name(s) to colour by (forwarded
#'         directly to \code{\link{spaghettiPlot}}).
#' }
#'
#' @param x a \code{std_campsis_tbl} object
#' @param variable name of the column to plot on the y-axis. Defaults to
#'   \code{"CONC"}. An informative error is raised when the column is absent.
#' @param colour stratification for line colour. One of \code{"auto"},
#'   \code{NULL}, or a character vector of column names. See Details.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{spaghettiPlot}}
#' @importFrom ggplot2 aes ggplot geom_line labs
#' @export
spaghetti_plot.std_campsis_tbl <- function(x, variable = "CONC", colour = "auto", ...) {
  .assert_variable_present(x, variable)

  if (identical(colour, "auto")) {
    colour <- .auto_colour_columns(x)
  }

  spaghettiPlot(x, variable = variable, colour = colour)
}
