
#_______________________________________________________________________________
#----                     shared S3 plot method helpers                     ----
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

#' Assert that a variable column is present in the data.
#'
#' Raises an informative error when the variable is absent.
#'
#' @param x a data frame
#' @param variable column name to look for
#' @return invisibly \code{NULL}; called for its side-effect
#' @keywords internal
.assert_variable_present <- function(x, variable) {
  if (!variable %in% colnames(x)) {
    available <- paste(colnames(x), collapse = ", ")
    stop(sprintf(
      "Column '%s' not found in data. Available columns: %s",
      variable, available
    ), call. = FALSE)
  }
  invisible(NULL)
}
