
#_______________________________________________________________________________
#----                         spaguetti_plot generic                        ----
#_______________________________________________________________________________

#' Spaghetti plot (S3 generic).
#'
#' @param x a CAMPSIS output object
#' @param ... additional arguments passed to the method
#' @return a ggplot object
#' @export
spaguetti_plot <- function(x, ...) {
  UseMethod("spaguetti_plot")
}

#_______________________________________________________________________________
#----                   spaguetti_plot.std_campsis_tbl                      ----
#_______________________________________________________________________________

#' Spaghetti plot for standard CAMPSIS simulation output.
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
#' @param x a \code{std_campsis_tbl} object, i.e. the direct output of
#'   \code{simulate()} with \code{DefaultOutfun()}
#' @param variable name of the column to plot on the y-axis. Defaults to
#'   \code{"CONC"}. An informative error is raised when the column is absent.
#' @param colour stratification for line colour. One of \code{"auto"},
#'   \code{NULL}, or a character vector of column names. See Details.
#' @param ... additional arguments (currently unused, reserved for future use)
#' @return a ggplot object
#' @seealso \code{\link{spaghettiPlot}}
#' @importFrom ggplot2 aes ggplot geom_line labs
#' @export
spaguetti_plot.std_campsis_tbl <- function(x, variable = "CONC",
                                           colour = "auto", ...) {
  # Resolve variable -------------------------------------------------------
  if (!variable %in% colnames(x)) {
    available <- paste(colnames(x), collapse = ", ")
    stop(sprintf(
      "Column '%s' not found in data. Available columns: %s",
      variable, available
    ), call. = FALSE)
  }

  # Resolve colour ---------------------------------------------------------
  if (identical(colour, "auto")) {
    colour <- .auto_colour_columns(x)
  }

  # Delegate to spaghettiPlot ----------------------------------------------
  spaghettiPlot(x, variable = variable, colour = colour)
}

#_______________________________________________________________________________
#----                         internal helpers                              ----
#_______________________________________________________________________________

#' Detect colour stratification columns automatically.
#'
#' Adds \code{"ARM"} when the column exists and contains more than one distinct
#' value. Same logic for \code{"SCENARIO"}.
#'
#' @param x a data frame (typically \code{std_campsis_tbl})
#' @return a character vector of column names to colour by, or \code{NULL}
#' @keywords internal
.auto_colour_columns <- function(x) {
  colour <- character(0)

  if ("ARM" %in% colnames(x) && dplyr::n_distinct(x$ARM) > 1) {
    colour <- c(colour, "ARM")
  }

  if ("SCENARIO" %in% colnames(x) && dplyr::n_distinct(x$SCENARIO) > 1) {
    colour <- c(colour, "SCENARIO")
  }

  if (length(colour) == 0) NULL else colour
}
