
#' Compute the prediction interval summary over time.
#' 
#' @param x data frame
#' @param output variable to show
#' @param scenarios scenarios, character vector, NULL is default
#' @param level PI level, default is 0.9 (90\% PI)
#' @param gather FALSE: med, low & up columns, TRUE: metric column
#' @return summary
#' @importFrom dplyr group_by_at rename_at summarise
#' @export
PI <- function(x, output, scenarios=NULL, level=0.90, gather=FALSE) {
  x <- factorScenarios(x, scenarios=scenarios)
  retValue <- x %>% dplyr::rename_at(.vars=output, .funs=function(x){"variable"}) %>%
    dplyr::group_by_at(c("time", scenarios)) %>%
    dplyr::summarise(
      med=median(variable),
      low=quantile(variable, (1-level)/2),
      up=quantile(variable, 1-(1-level)/2)
    )
  # Gather data if requested
  if (gather) {
    
    # Remove attributes in columns low, med, up (5%, 95%, coming from the quantile method)
    # This causes warnings
    retValue <- retValue %>% dplyr::mutate(dplyr::across(c("low", "med", "up"), as.vector))
    
    if (is.null(scenarios)) {
      retValue <- retValue %>% tidyr::gather(key="metric", value="value", -time)
    } else {
      retValue <- retValue %>% tidyr::gather(key="metric", value="value", -time, -dplyr::all_of(scenarios))
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
#' @return VPC summary
#' @importFrom dplyr across all_of mutate
#' @importFrom tidyr gather
#' @export
VPC <- function(x, scenarios=NULL, level=0.90) {
  x <- factorScenarios(x, scenarios=scenarios)
  retValue <- PI(x=x, output="value", scenarios=c("metric", scenarios), level=level)
  return(retValue)
}