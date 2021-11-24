
#' Compute the prediction interval summary over time.
#' 
#' @param x data frame
#' @param output variable to show, character value
#' @param scenarios scenarios, character vector, NULL is default
#' @param level PI level, default is 0.9 (90\% PI)
#' @param gather FALSE: med, low & up columns, TRUE: metric column
#' @return summary
#' @importFrom dplyr across group_by_at mutate rename_at summarise
#' @importFrom tidyr gather
#' @export
PI <- function(x, output, scenarios=NULL, level=0.90, gather=TRUE) {
  assertthat::assert_that(is.character(output) && length(output)==1)
  x <- factorScenarios(x, scenarios=scenarios)
  retValue <- x %>% dplyr::rename_at(.vars=output, .funs=~"variable_") %>%
    dplyr::group_by_at(c("TIME", scenarios)) %>%
    dplyr::summarise(
      med=median(variable_),
      low=quantile(variable_, (1-level)/2),
      up=quantile(variable_, 1-(1-level)/2)
    )
  # Gather data if requested
  if (gather) {
    # Remove attributes in columns low, med, up (5%, 95%, coming from the quantile method)
    # This causes warnings
    retValue <- retValue %>% dplyr::mutate(dplyr::across(c("low", "med", "up"), as.vector))
    
    if (is.null(scenarios)) {
      retValue <- retValue %>% tidyr::gather(key="metric", value="value", -TIME)
    } else {
      retValue <- retValue %>% tidyr::gather(key="metric", value="value", -TIME, -dplyr::all_of(scenarios))
    }
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
#' @param scenarios scenarios, character vector, NULL is default
#' @param level PI level, default is 0.9 (90\% PI)
#' @importFrom tidyr pivot_wider
#' @return VPC summary with columns TIME, <scenarios> and all combinations of 
#' low, med, up (i.e. low_low, low_med, low_up, etc.) 
#' @export
VPC <- function(x, scenarios=NULL, level=0.90) {
  x_ <- factorScenarios(x, scenarios=scenarios)
  retValue <- PI(x=x_, output="value", scenarios=c("metric", scenarios), level=level, gather=FALSE)
  retValue_ <- retValue %>% tidyr::pivot_wider(names_from=metric, names_glue="{metric}_{.value}", values_from=c(low, med, up))
  return(retValue_)
}