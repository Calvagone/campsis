
#' Factor scenarios columns if not done yet.
#' 
#' @param x data frame
#' @param scenarios scenarios
#' @importFrom dplyr mutate_at
#' @keywords internal
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

#' Filter CAMPSIS output on observation rows.
#' 
#' @param x data frame, CAMPSIS output
#' @importFrom dplyr filter
#' @export
obsOnly <- function(x) {
  if ("EVID" %in% colnames(x)) {
    return(x %>% dplyr::filter(EVID==0))
  } else {
    return(x)
  }
}

#' Filter CAMPSIS output on dosing rows.
#' 
#' @param x data frame, CAMPSIS output
#' @importFrom dplyr filter
#' @export
dosingOnly <- function(x) {
  if ("EVID" %in% colnames(x)) {
    return(x %>% dplyr::filter(EVID==1))
  } else {
    return(x)
  }
}

#' Spaghetti plot.
#' 
#' @param x data frame
#' @param output variable to show
#' @param scenarios scenarios
#' @return plot
#' @importFrom ggplot2 aes_string ggplot geom_line
#' @export
spaghettiPlot <- function(x, output, scenarios=NULL) {
  hasId <- "ID" %in% colnames(x)
  x <- factorScenarios(x %>% obsOnly(), scenarios=scenarios)
  if (hasId) {
    if (length(scenarios) > 0) {
      colour <- paste0(scenarios, collapse = ":")
      group <- paste0("interaction(", paste0(c("ID", scenarios), collapse=","), ")")
    } else {
      colour <- NULL
      group <- "ID"
    }
    plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="TIME", y=output, group=group, colour=colour)) +
      ggplot2::geom_line()
  } else {
    plot <- ggplot2::ggplot(x, ggplot2::aes_string(x="TIME", y=output)) +
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
  x <- PI(x=x %>% obsOnly(), output=output, scenarios=scenarios, level=level, gather=FALSE)
  if (length(scenarios) > 0) {
    colour <- paste0(scenarios, collapse = ":")
  } else {
    colour <- NULL
  }
  plot <- ggplot2::ggplot(data=x, mapping=ggplot2::aes_string(x="TIME", colour=colour)) +
    ggplot2::geom_line(ggplot2::aes(y=med)) +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin="low", ymax="up", colour=colour, fill=colour), colour=NA, alpha=0.25)
  plot <- plot + ggplot2::ylab(output)
  return(plot)
}

#' VPC plot (1 plot per scenario).
#' 
#' @param x data frame, output of pmxsim with replicates
#' @param scenarios scenarios, character vector, NULL is default
#' @param level PI level, default is 0.9 (90\% PI)
#' @return plot
#' @importFrom dplyr filter_at pull
#' @export
vpcPlot <- function(x, scenarios=NULL, level=0.90) {
  if (length(scenarios) > 1) {
    stop("Currently max 1 scenario allowed")
  }
  x <- VPC(x=x %>% obsOnly(), scenarios=scenarios, level=level)

  if (length(scenarios) == 0) {
    retValue <- vpcPlotDelegate(x)
  } else {
    retValue <- list()
    scenario <- scenarios[1]
    values <- unique(x %>% dplyr::pull(scenario)) %>% as.character()
    for (valueIndex in seq_along(values)) {
      value <- values[valueIndex]
      retValue[[valueIndex]] <- vpcPlotDelegate(x %>% dplyr::filter_at(.vars=scenario, .vars_predicate=~.x==value))
    }
  }
  
  return(retValue)
}

#' VPC plot delegate.
#' 
#' @param summary from vpcPlot
#' @return plot
#' @importFrom ggplot2 aes ggplot geom_line geom_ribbon ylab
#' @keywords internal
#' 
vpcPlotDelegate <- function(summary) {
  summary.low <- summary %>% dplyr::filter(metric=="low")
  summary.med <- summary %>% dplyr::filter(metric=="med")
  summary.up <- summary %>% dplyr::filter(metric=="up")
  
  plot <- ggplot2::ggplot(summary.med, ggplot2::aes(x=TIME, y=med)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=low, ymax=up), alpha=0.15, color=NA, fill="red") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=low, ymax=up), data=summary.low, alpha=0.15, color=NA, fill="blue") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=low, ymax=up), data=summary.up, alpha=0.15, color=NA, fill="blue") +
    ggplot2::ylab("")
  return(plot)
}

