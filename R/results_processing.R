
allStrataLevels <- function() {
  return("all")
}

getDefaultStrata <- function() {
  return(c(SCENARIO=allStrataLevels(), ARM=allStrataLevels()))
}

filterOutputOnStrata <- function(x, strata) {
  # Detect the specific strata
  specific_strata <- strata[strata != allStrataLevels()]
  
  # Filter input data frame to specific strata
  x_reduced <- purrr::reduce(
    names(specific_strata),
    ~ dplyr::filter(.x, .data[[.y]] == specific_strata[[.y]]),
    .init = x
  )
  return(x_reduced)
}


#' Compute the prediction interval summary over time.
#' 
#' @param x data frame
#' @param variable variable(s) used to compute the prediction interval, character vector
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @param gather FALSE: med, low & up columns, TRUE: metric column
#' @return a summary table
#' @importFrom dplyr across all_of group_by_at mutate rename_at summarise
#' @importFrom tidyr pivot_longer
#' @importFrom stats median quantile
#' @export
PI <- function(x, variable, strata = getDefaultStrata(), level = 0.90, gather = TRUE) {
  assertthat::assert_that(is.character(variable) && length(variable) == 1,
   msg = "variable must be a character vector of length 1")
  assertthat::assert_that(
    is.null(strata) || (is.atomic(strata) && !is.null(names(strata)) && all(nzchar(names(strata)))),
    msg = "strata must be a fully named vector or NULL")

  # Only keep strata columns that are actually present in x
  strata_cols <- names(strata)
  strata_cols <- strata_cols[strata_cols %in% colnames(x)]

  # Calculate prediction intervals
  retValue <- filterOutputOnStrata(x = x, strata = strata) %>%
    dplyr::rename(variable_ = dplyr::all_of(variable)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("TIME", strata_cols)))) %>%
    dplyr::summarise(
      med = stats::median(.data$variable_, na.rm = TRUE),
      low = stats::quantile(.data$variable_, (1 - level) / 2, names = FALSE, na.rm = TRUE),
      up = stats::quantile(.data$variable_, 1 - (1 - level) / 2, names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )

  # Gather data if requested
  if (gather) {
    retValue <- retValue %>%
      tidyr::pivot_longer(
        cols = c("med", "low", "up"), 
        names_to = "metric", 
        values_to = "value"
      )
  }

  return(retValue)
}

#' Compute the VPC summary. Input data frame must contain the following columns:
#' - replicate: replicate number
#' - low: low percentile value in replicate (and in scenario if present)
#' - med: median value in replicate (and in scenario if present)
#' - up: up percentile value in replicate (and in scenario if present)
#' - any scenario column
#' 
#' @param x data frame
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @importFrom tidyr pivot_wider
#' @return VPC summary with columns TIME, stratification variables and all combinations of 
#' low, med, up (i.e. low_low, low_med, low_up, etc.) 
#' @export
VPC <- function(x, strata=NULL, level=0.90) {
  retValue <- PI(x=x, variable="value", strata=c(metric=allStrataLevels(), strata), level=level, gather=FALSE)
  retValue_ <- retValue %>%
    tidyr::pivot_wider(names_from="metric",
                       names_glue="{metric}_{.value}",
                       values_from=c("low", "med", "up"))
  return(retValue_)
}
