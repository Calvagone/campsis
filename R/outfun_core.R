
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

metrics_pivot_longer <- function(x, cols) {
  x <- x |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols),
        names_to = "metric",
        values_to = "value"
      )
  return(x)
}

metrics_pivot_wider <- function(x) {
  x <- x |>
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "value"
    )
  return(x)
}

#' Compute the prediction interval summary over time.
#' 
#' @param x data frame
#' @param variable variable(s) used to compute the prediction interval, character vector.
#'   When more than one variable is supplied, a \code{variable} column is added to the
#'   output to identify each one.
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @return a summary table
#' @importFrom dplyr across all_of bind_rows mutate group_by summarise
#' @importFrom tidyr pivot_longer
#' @importFrom stats median quantile
#' @export
compute_pi <- function(x, variable, strata = getDefaultStrata(), level = 0.90) {
  assertthat::assert_that(
    is.character(variable) && length(variable) >= 1,
    msg = "variable must be a non-empty character vector"
  )
  assertthat::assert_that(
    is.null(strata) || (is.atomic(strata) && !is.null(names(strata)) && all(nzchar(names(strata)))),
    msg = "strata must be a fully named vector or NULL"
  )

  # Only keep strata columns that are actually present in x
  strata_cols <- names(strata)[names(strata) %in% colnames(x)]
  group_cols  <- c(strata_cols, "TIME")

  assertthat::assert_that(
    !"variable" %in% strata_cols,
    msg = "variable can't be used as a stratification column name"
  )
  
  # Filter data
  x_filtered <- filterOutputOnStrata(x = x, strata = strata)

  # Pre-calculate quantile probabilities
  prob_low <- (1 - level) / 2
  prob_up  <- 1 - prob_low

  # Pivot the variables long first, keeping data grouped properly
  res <- x_filtered |>
    dplyr::select(dplyr::all_of(c(group_cols, variable))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(variable), 
      names_to = "variable", 
      values_to = "value"
    ) |>
    # Group by the grouping columns AND the new variable column
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "variable")))) |>
    # Summarize directly into clean columns
    dplyr::summarise(
      med = stats::median(.data$value, na.rm = TRUE),
      low = stats::quantile(.data$value, prob_low, names = FALSE, na.rm = TRUE),
      up  = stats::quantile(.data$value, prob_up,  names = FALSE, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot longer by default
  res <- metrics_pivot_longer(x = res, cols = c("med", "low", "up"))

  # Variable before TIME
  res <- res |>
    dplyr::relocate("variable", .before = "TIME")

  return(res)
}

#' Compute the VPC summary. Input data frame must contain the following columns:
#' - replicate: replicate number
#' - low: low percentile value in replicate (and in scenario if present)
#' - med: median value in replicate (and in scenario if present)
#' - up: up percentile value in replicate (and in scenario if present)
#' - any scenario column
#' 
#' @param x data frame, with metric
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @importFrom tidyr pivot_wider
#' @return VPC summary with columns TIME, stratification variables and all combinations of 
#' low, med, up (i.e. low_low, low_med, low_up, etc.) 
#' @export
VPC <- function(x, strata=NULL, level=0.90) {
  x <- x |>
    dplyr::rename(original_metric=metric)
  retValue <- compute_pi(x=x, variable="value", strata=c(original_metric=allStrataLevels(), strata), level=level) |>
    metrics_pivot_wider()

  retValue_ <- retValue |>
    tidyr::pivot_wider(names_from="original_metric",
                       names_glue="{original_metric}_{.value}",
                       values_from=c("low", "med", "up"))
  return(retValue_)
}
