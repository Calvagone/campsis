
#' Compute the prediction interval summary over time.
#' 
#' @param x data frame
#' @param output variable to show
#' @param scenarios scenarios
#' @param level PI level, default is 0.9 (90\% PI)
#' @return summary
#' @importFrom dplyr group_by_at rename_at summarise
#' @export
PI <- function(x, output, scenarios=NULL, level=0.90) {
  x <- factorScenarios(x, scenarios=scenarios)
  retValue <- x %>% dplyr::rename_at(.vars=output, .funs=function(x){"variable"}) %>%
    dplyr::group_by_at(c("time", scenarios)) %>%
    dplyr::summarise(
      med=median(variable),
      low=quantile(variable, (1-level)/2),
      up=quantile(variable, 1-(1-level)/2)
    )
  return(retValue)
}