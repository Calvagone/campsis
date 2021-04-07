
#' Factor scenarios columns if not done yet.
#' 
#' @param x data frame
#' @param scenarios scenarios
#' @importFrom dplyr mutate_at
#' @export
factorScenarios <- function(x, scenarios=NULL) {
  if (length(scenarios) > 0) {
    return(x %>% dplyr::mutate_at(.vars=scenarios, .funs=function(col){
      if (!is.factor(col)) {
        return(factor(col))
      } else {
        return(col)
      }
    }))
  } else {
    return(x)
  }
}

#' Spaguetti plot.
#' 
#' @param x data frame
#' @param output variable to show
#' @param scenarios scenarios
#' @return plot
#' @importFrom ggplot2 aes_string ggplot geom_line
#' @export
spaguettiPlot <- function(x, output, scenarios=NULL) {
  hasId <- "id" %in% colnames(x)
  x <- factorScenarios(x, scenarios=scenarios)
  if (hasId) {
    if (length(scenarios) > 0) {
      colour <- paste0(scenarios, collapse = ":")
    } else {
      colour <- NULL
    }
    plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="time", y=output, group="id", colour=colour)) +
      ggplot2::geom_line()
  } else {
    plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="time", y=output)) +
      ggplot2::geom_line()
  }
  return(plot)
}

#' Shaded plot (or prediction interval plot).
#' 
#' @param x data frame
#' @param output variable to show
#' @param scenarios scenarios
#' @param level PI level, default is 0.9 (90\% PI)
#' @return plot
#' @importFrom ggplot2 aes aes_string ggplot geom_line geom_ribbon
#' @export
shadedPlot <- function(x, output, scenarios=NULL, level=0.90) {
  x <- PI(x=x, output=output, scenarios=scenarios, level=level)
  if (length(scenarios) > 0) {
    colour <- paste0(scenarios, collapse = ":")
  } else {
    colour <- NULL
  }
  plot <- ggplot2::ggplot(data=x, aes=ggplot2::aes_string(x="time", colour=colour)) +
          ggplot2::geom_line(ggplot2::aes(y=med)) +
          ggplot2::geom_ribbon(ggplot2::aes_string(ymin="low", ymax="up", colour=colour, fill=colour), colour=NA, alpha=0.25)
  plot <- plot + ggplot2::ylab(output)
  return(plot)
}
