#'
#' Import the whole campsismod package into NAMESPACE when parsed by 'roxygen'.
#'
#' @import campsismod
#' @keywords internal
#' @return always TRUE
#'
import_campsismod_to_namespace <- function() {
  return(TRUE)
}

#'
#' Convert user-given distribution to an explicit Campsis distribution.
#' Passed distribution can be:
#' - a NULL value. In that case, it will be converted into an 'UndefinedDistribution'.
#' - a single numeric value. In that case, it will be converted into a 'ConstantDistribution'.
#' - a numeric vector. In that case, it will be converted into a 'FixedDistribution'.
#' - all available types of distribution. In this case, no conversion is applied.
#'
#' @param distribution user-given distribution
#' @return a distribution object
#' @keywords internal
#'
to_explicit_distribution <- function(distribution) {
  if (is.null(distribution)) {
    return(new("undefined_distribution"))
  } else if (is.numeric(distribution)) {
    if (distribution %>% length() > 1) {
      return(FixedDistribution(distribution))
    } else {
      return(ConstantDistribution(distribution))
    }
  } else if (is(distribution, "distribution")) {
    return(distribution)
  } else {
    stop("Not a distribution nor a numeric value")
  }
}

to_explicit_distribution_list <- function(distribution, cmtNo) {
  if (is.null(distribution)) {
    retValue <- list(new("undefined_distribution"))
  }
  if (is.numeric(distribution)) {
    # E.g. f=c(0.5, 1)
    retValue <- distribution %>% purrr::map(to_explicit_distribution)
  } else if (is.list(distribution)) {
    # E.g. f=list(0.5, 1)
    retValue <- distribution %>% purrr::map(to_explicit_distribution)
  } else {
    retValue <- list(to_explicit_distribution(distribution))
  }
  size <- length(retValue)
  if (size == cmtNo) {
    return(retValue)
  } else if (size == 1 && cmtNo > 1) {
    return(rep(retValue, cmtNo))
  } else {
    stop("Invalid distribution")
  }
}

env_var_is_true <- function(x) {
  return(isTRUE(as.logical(Sys.getenv(x, "false"))))
}

#'
#' Check if the current session is on CRAN. The objective is to potentially suppress
#' long tasks to be run on CRAN (long tests or vignettes).
#'
#' @return logical value TRUE/FALSE
#' @export
#' @keywords internal
on_cran <- function() {
  # Copied from testthat:::on_cran()
  return(!interactive() && !env_var_is_true("NOT_CRAN"))
}

#'
#' Check if the current session is on CI (e.g. GitHub actions).
#'
#' @return logical value TRUE/FALSE
#' @export
#' @keywords internal
on_ci <- function() {
  # Copied from testthat:::on_ci()
  return(env_var_is_true("CI"))
}

#'
#' Get the Campsis options (R options).
#'
#' @return global options for Campsis
#' @export
#' @keywords internal
get_campsis_options <- function() {
  return(getOption("campsis.options"))
}

#'
#' Get Campsis option logic.
#'
#' @param name option to search
#' @param default default value if option not found
#' @return option value
#' @export
get_campsis_option <- function(name, default) {
  option <- get_campsis_options()
  if (is.null(option)) {
    return(default)
  } else {
    value <- option[[name]]
    if (is.null(value)) {
      return(default)
    } else {
      return(value)
    }
  }
}

#' Preserve Existing Column Value Order as Factor Levels
#'
#' Converts target columns into factors using their current unique row appearance
#' order as the factor levels.
#'
#' @param x A data frame or tibble.
#' @param cols A character vector of column names to convert.
#'
#' @return A data frame with updated factor columns.
#' @export
#' @importFrom dplyr mutate across all_of
preserve_column_levels <- function(x, cols) {
  target_cols <- intersect(cols, colnames(x))
  if (length(target_cols) > 0) {
    x <- x %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(target_cols),
          ~ factor(.x, levels = unique(.x))
        )
      )
  }
  return(x)
}

#' Strip Factor Class from Columns
#'
#' Converts target columns from factors into standard character vectors.
#'
#' @param x A data frame or tibble.
#' @param cols A character vector of column names to convert.
#'
#' @return A data frame with character columns.
#' @export
#' @importFrom dplyr mutate across all_of
remove_column_levels <- function(x, cols) {
  target_cols <- intersect(cols, colnames(x))
  if (length(target_cols) > 0) {
    x <- x %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(target_cols),
          as.character
        )
      )
  }
  return(x)
}
