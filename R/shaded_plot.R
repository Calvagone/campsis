#_______________________________________________________________________________
#----                         shaded_plot generic                           ----
#_______________________________________________________________________________

#' Shaded plot / prediction interval plot (S3 generic).
#'
#' @param x a CAMPSIS output object
#' @param ... additional arguments passed to the method
#' @return a ggplot object
#' @export
shaded_plot <- function(x, ...) {
  UseMethod("shaded_plot")
}

#_______________________________________________________________________________
#----                   shaded_plot.std_campsis_tbl                         ----
#_______________________________________________________________________________

#' Shaded plot for standard CAMPSIS simulation output.
#'
#' Plots a median line with a prediction interval ribbon from a
#' \code{std_campsis_tbl} (the default output of \code{simulate()} when no
#' custom \code{outfun} is used).
#'
#' Colour stratification is resolved as follows:
#' \itemize{
#'   \item \code{"auto"} (default): \code{ARM} is added when more than one
#'         distinct arm is detected in the data; \code{SCENARIO} is added when
#'         more than one distinct scenario is detected.
#'   \item \code{NULL}: no colour mapping.
#'   \item character vector: explicit column name(s) to colour/fill by
#'         (forwarded directly to \code{\link{shadedPlot}}).
#' }
#'
#' @param x a \code{std_campsis_tbl} object
#' @param variable name of the column to summarise on the y-axis. Defaults to
#'   \code{"CONC"}. An informative error is raised when the column is absent.
#' @param colour stratification for line and ribbon colour. One of
#'   \code{"auto"}, \code{NULL}, or a character vector of column names.
#'   See Details.
#' @param strat_extra additional column name(s) used for stratification in the
#'   prediction interval computation but \emph{not} mapped to colour — useful
#'   when combined with \code{facet_wrap()}. Forwarded to
#'   \code{\link{shadedPlot}}. Default is \code{NULL}.
#' @param level prediction interval level. Default is \code{0.90} (90\% PI).
#' @param alpha transparency of the ribbon. Default is \code{0.25}.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{shadedPlot}}
#' @importFrom ggplot2 aes ggplot geom_line geom_ribbon labs ylab
#' @export
shaded_plot.std_campsis_tbl <- function(
  x,
  variable = "CONC",
  colour = "auto",
  strat_extra = NULL,
  level = 0.90,
  alpha = 0.25,
  ...
) {
  .assert_variable_present(x, variable)

  if (identical(colour, "auto")) {
    colour <- .auto_colour_columns(x)
  }

  shadedPlot(x, variable = variable, colour = colour, strat_extra = strat_extra, level = level, alpha = alpha)
}
