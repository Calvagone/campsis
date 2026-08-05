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

#' Assert that one or more variable columns are present in the data.
#'
#' Raises an informative error listing all absent column names.
#'
#' @param x a data frame
#' @param variable character vector of column name(s) to check
#' @return invisibly \code{NULL}; called for its side-effect
#' @keywords internal
.assert_variable_present <- function(x, variable) {
  missing <- setdiff(variable, colnames(x))
  if (length(missing) > 0) {
    available <- paste(colnames(x), collapse = ", ")
    stop(
      sprintf(
        "Column(s) '%s' not found in data. Available columns: %s",
        paste(missing, collapse = "', '"),
        available
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Does the data contain more than one replicate?
#'
#' @return \code{TRUE} if the data contains a \code{replicate} column with more than one distinct value, \code{FALSE} otherwise
#' @param x a data frame (typically \code{std_campsis_tbl})
#' @importFrom dplyr n_distinct
#' @keywords internal
.is_replicated <- function(x) {
  return("replicate" %in% colnames(x) && dplyr::n_distinct(x$replicate) > 1)
}
