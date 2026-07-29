#' @importFrom rlang expr
all_strata_levels <- function() {
  return("all")
}

get_default_strata <- function() {
  return(c(SCENARIO=all_strata_levels(), ARM=all_strata_levels()))
}

#' @importFrom purrr reduce
#' @importFrom dplyr filter
filter_output_on_strata <- function(x, strata) {
  # Detect the specific strata
  specific_strata <- strata[strata != all_strata_levels()]
  
  # Filter input data frame to specific strata
  x_reduced <- purrr::reduce(
    names(specific_strata),
    ~ dplyr::filter(.x, .data[[.y]] == specific_strata[[.y]]),
    .init = x
  )
  return(x_reduced)
}

#' @importFrom tidyr pivot_longer
#' @importFrom dplyr all_of
metrics_pivot_longer <- function(x, cols) {
  x <- x |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(cols),
        names_to = "metric",
        values_to = "value"
      )
  return(x)
}

#' @importFrom tidyr pivot_wider
metrics_pivot_wider <- function(x) {
  x <- x |>
    tidyr::pivot_wider(
      names_from = "metric",
      values_from = "value"
    )
  return(x)
}

#' Compute generic statistics over time.
#' 
#' @param x data frame
#' @param variable variable(s) used to compute the statistics, character vector.
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#' @param stats character vector of statistics to compute. Supported: "median", "mean", or percentiles like "p5", "p95", "p2.5", "p97.5".
#' @return a summary table in long format
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom rlang expr set_names
#' @importFrom stats median quantile
#' @importFrom dplyr all_of group_by across summarise relocate
#' @importFrom tidyr pivot_longer
#' @export
compute_stats <- function(x, variable, strata = get_default_strata(), stats = c("p5", "median", "p95")) {
  assertthat::assert_that(
    is.character(variable) && length(variable) >= 1,
    msg = "variable must be a non-empty character vector"
  )
  assertthat::assert_that(
    is.null(strata) || (is.atomic(strata) && !is.null(names(strata)) && all(nzchar(names(strata)))),
    msg = "strata must be a fully named vector or NULL"
  )
  assertthat::assert_that(
    is.character(stats) && length(stats) >= 1,
    msg = "stats must be a non-empty character vector"
  )

  # Only keep strata columns that are actually present in x
  strata_cols <- names(strata)[names(strata) %in% colnames(x)]
  group_cols  <- c(strata_cols, "TIME")

  assertthat::assert_that(
    !"variable" %in% strata_cols,
    msg = "variable can't be used as a stratification column name"
  )
  
  # Filter data
  x_filtered <- filter_output_on_strata(x = x, strata = strata)

  # Map string shortcuts to an expression list for summarize()
  # This dynamically builds the calls for mean, median, or any pXX quantile
  summary_exprs <- purrr::map(stats, function(stat) {
    if (stat == "median") {
      rlang::expr(stats::median(.data$value, na.rm = TRUE))
    } else if (stat == "mean") {
      rlang::expr(mean(.data$value, na.rm = TRUE))
    } else if (grepl("^p[0-9]+(\\.[0-9]+)?$", stat)) {
      # Extract digits for percentile (e.g., "p95" -> 0.95, "p2.5" -> 0.025)
      prob <- as.numeric(sub("p", "", stat)) / 100
      rlang::expr(stats::quantile(.data$value, !!prob, names = FALSE, na.rm = TRUE))
    } else {
      stop(paste("Unsupported statistic:", stat))
    }
  }) |> rlang::set_names(stats)

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
    # Dynamically summarize based on user requests
    dplyr::summarise(!!!summary_exprs, .groups = "drop")
  
  # Pivot longer into the final format
  res <- metrics_pivot_longer(x = res, cols = stats)

  # Variable before TIME
  res <- res |>
    dplyr::relocate("variable", .before = "TIME")

  return(res)
}

#' Compute the prediction interval summary over time.
#' 
#' @param x data frame
#' @param variable variable(s) used to compute the prediction interval, character vector.
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#' @param level PI level, default is 0.9 (90\% PI)
#' @return a summary table
#' @importFrom dplyr mutate case_when
#' @export
compute_pi <- function(x, variable, strata = get_default_strata(), level = 0.90) {
  # Map PI level to requested percentile strings
  prob_low <- (1 - level) / 2
  prob_up  <- 1 - prob_low
  
  low_name <- paste0("p", prob_low * 100)
  up_name  <- paste0("p", prob_up * 100)
  
  # Call the generic function
  res <- compute_stats(
    x = x, 
    variable = variable, 
    strata = strata, 
    stats = c(low_name, "median", up_name)
  )
  
  # Map the generic metric names ("p5", "median", "p95") back to ("low", "med", "up")
  res <- res |> 
    dplyr::mutate(metric = dplyr::case_when(
      metric == "median" ~ "med",
      metric == low_name ~ "low",
      metric == up_name  ~ "up",
      TRUE               ~ metric
    ))
  
  return(res)
}

#' Make the VPC summary. Input data frame must contain the following columns:
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
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider
#' @return VPC summary with columns TIME, stratification variables and all combinations of 
#' low, med, up (i.e. low_low, low_med, low_up, etc.)
#' @keywords internal
make_vpc_summary <- function(x, strata=NULL, level=0.90) {
  x <- x |>
    dplyr::rename(original_metric="metric")
  retValue <- compute_pi(x=x, variable="value", strata=c(original_metric=all_strata_levels(), strata), level=level) |>
    metrics_pivot_wider()

  retValue_ <- retValue |>
    tidyr::pivot_wider(names_from="original_metric",
                       names_glue="{original_metric}_{.value}",
                       values_from=c("low", "med", "up"))
  return(retValue_)
}
