
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
#' @return a data frame with the observation rows
#' @importFrom dplyr filter
#' @export
obsOnly <- function(x) {
  if ("EVID" %in% colnames(x)) {
    return(x %>% dplyr::filter(.data$EVID==0))
  } else {
    return(x)
  }
}

#' Filter CAMPSIS output on dosing rows.
#' 
#' @param x data frame, CAMPSIS output
#' @return a data frame with the dosing rows
#' @importFrom dplyr filter
#' @export
dosingOnly <- function(x) {
  if ("EVID" %in% colnames(x)) {
    return(x %>% dplyr::filter(.data$EVID==1))
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
#' @param alpha alpha parameter (transparency) given to geom_ribbon
#' @return a ggplot object
#' @importFrom ggplot2 aes_string ggplot geom_line geom_ribbon ylab
#' @export
shadedPlot <- function(x, output, scenarios=NULL, level=0.90, alpha=0.25) {
  x <- PI(x=x %>% obsOnly(), output=output, scenarios=scenarios, level=level, gather=FALSE)
  if (length(scenarios) > 0) {
    colour <- paste0(scenarios, collapse = ":")
  } else {
    colour <- NULL
  }
  plot <- ggplot2::ggplot(data=x, mapping=ggplot2::aes_string(x="TIME", colour=colour)) +
    ggplot2::geom_line(ggplot2::aes_string(y="med")) +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin="low", ymax="up", colour=colour, fill=colour), colour=NA, alpha=alpha)
  plot <- plot + ggplot2::ylab(output)
  return(plot)
}

#' VPC plot.
#' 
#' @param x data frame, output of CAMPSIS with replicates
#' @param scenarios scenarios, character vector, NULL is default
#' @param level PI level, default is 0.9 (90\% PI)
#' @param alpha alpha parameter (transparency) given to geom_ribbon
#' @return a ggplot object
#' @importFrom dplyr all_of
#' @importFrom ggplot2 aes_string facet_wrap ggplot ylab
#' @export
vpcPlot <- function(x, scenarios=NULL, level=0.90, alpha=0.15) {
  if (length(scenarios) > 1) {
    stop("Currently max 1 scenario allowed")
  }
  summary <- VPC(x=x, scenarios=scenarios, level=level)

  plot <- ggplot2::ggplot(summary, ggplot2::aes_string(x="TIME", group=scenarios)) +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin="med_low", ymax="med_up"), alpha=alpha, color=NA, fill="red") +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin="low_low", ymax="low_up"), alpha=alpha, color=NA, fill="blue") +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin="up_low", ymax="up_up"), alpha=alpha, color=NA, fill="blue") +
    ggplot2::ylab("")
  
  return(plot)
}
