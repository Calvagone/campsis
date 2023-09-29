
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

#' Unite the given column names.
#' 
#' @param x data frame, CAMPSIS output
#' @param columns columns to unify
#' @param colname destination column name
#' @param factor factor the destination column
#' @return a data frame
#' @importFrom dplyr all_of
#' @importFrom tidyr unite
#' @keywords internal
uniteColumns <- function(x, columns, colname, factor=TRUE) {
  x <- x %>%
    tidyr::unite(!!colname, dplyr::all_of(columns), remove=FALSE, sep=" / ")
  if (factor) {
    x <- x %>%
      dplyr::mutate(!!colname := factor(.data[[colname]], levels=unique(.data[[colname]])))
  }
  return(x)
}

#' Get data of given column unless if does not exist (return NULL in that case).
#' 
#' @param .data data frame
#' @param colname column name
#' @return a vector
#' @keywords internal
getColumn <- function(.data, colname) {
  if (is.null(colname)) {
    return(NULL)
  } else {
    return(.data[[colname]])
  }
}

#' Spaghetti plot.
#' 
#' @param x data frame
#' @param output variable to show
#' @param scenarios scenarios
#' @return plot
#' @importFrom ggplot2 aes ggplot geom_line
#' @export
spaghettiPlot <- function(x, output, scenarios=NULL) {
  group <- "GROUP_GGPLOT"
  x <- uniteColumns(x=x %>% obsOnly(), columns=c("ID", scenarios), colname=group)
  
  if (length(scenarios) > 0) {
    colour <- "COLOUR_GGPLOT"
    x <- uniteColumns(x=x, columns=scenarios, colname=colour)
  } else {
    colour <- NULL
  }
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=.data$TIME, y=.data[[output]], group=.data[[group]], colour=getColumn(.data, colour))) +
    ggplot2::geom_line()
  
  if (length(scenarios) > 0) {
    plot <- plot + ggplot2::labs(colour=paste0(scenarios, collapse = " / "))
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
#' @importFrom ggplot2 aes ggplot geom_line geom_ribbon ylab
#' @export
shadedPlot <- function(x, output, scenarios=NULL, level=0.90, alpha=0.25) {
  if (length(scenarios) > 0) {
    colour <- "COLOUR_GGPLOT"
    x <- uniteColumns(x=x %>% obsOnly(), columns=scenarios, colname=colour)
  } else {
    colour <- NULL
  }
  x <- PI(x=x, output=output, scenarios=c(scenarios, colour), level=level, gather=FALSE)

  plot <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=.data$TIME, colour=getColumn(.data, colour))) +
    ggplot2::geom_line(ggplot2::aes(y=.data$med)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$low, ymax=.data$up, colour=getColumn(.data, colour), fill=getColumn(.data, colour)), colour=NA, alpha=alpha) +
    ggplot2::ylab(output)
  
  if (length(scenarios) > 0) {
    plot <- plot + ggplot2::labs(colour=paste0(scenarios, collapse = " / "),
                                 fill=paste0(scenarios, collapse = " / "))
  }

  return(plot)
}

#' Scatter plot (or X vs Y plot).
#' 
#' @param x data frame
#' @param output the 2 variables to show, character vector
#' @param scenarios scenarios
#' @param time the time to look at those 2 variables, if NULL, min time is used (usually 0)
#' @return a ggplot object
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes ggplot geom_point
#' @export
scatterPlot <- function (x, output, scenarios=NULL, time=NULL) {
  group <- "GROUP_GGPLOT"
  x <- uniteColumns(x=x %>% obsOnly(), columns=c("ID", scenarios), colname=group)
  
  if (is.null(time)) {
    time <- min(x$TIME)
  }
  x <- x %>% dplyr::filter(.data$TIME %in% time)
  
  if (output %>% length() == 1) {
    x$MY_OUTPUT_2 <- 0
    output <- c(output, "MY_OUTPUT_2")
  } else if (output %>% length() > 2) {
    stop("Please provide 2 outputs at most !")
  }

  if (length(scenarios) > 0) {
    colour <- "COLOUR_GGPLOT"
    x <- uniteColumns(x=x, columns=scenarios, colname=colour)
  } else {
    colour <- NULL
  }
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=.data[[output[1]]], y=.data[[output[2]]], group=.data[[group]], colour=getColumn(.data, colour))) +
    ggplot2::geom_point()
  
  if (length(scenarios) > 0) {
    plot <- plot + ggplot2::labs(colour=paste0(scenarios, collapse = " / "))
  }
  
  return(plot)
}

#' VPC plot.
#' 
#' @param x data frame, output of CAMPSIS with replicates
#' @param scenarios scenarios, character vector, NULL is default
#' @param level PI level, default is 0.9 (90\% PI)
#' @param alpha alpha parameter (transparency) given to geom_ribbon
#' @return a ggplot object
#' @importFrom ggplot2 aes ggplot ylab
#' @export
vpcPlot <- function(x, scenarios=NULL, level=0.90, alpha=0.15) {
  if (length(scenarios) > 1) {
    stop("Currently max 1 scenario allowed")
  }
  summary <- VPC(x=x, scenarios=scenarios, level=level)
  if (length(scenarios) > 0) {
    group <- "GROUP_GGPLOT"
    summary <- uniteColumns(x=x, columns=scenarios, colname=group)
  } else {
    group <- NULL
  }

  plot <- ggplot2::ggplot(summary, ggplot2::aes(x=.data$TIME, group=getColumn(.data, group))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$med_low, ymax=.data$med_up), alpha=alpha, color=NA, fill="red") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$low_low, ymax=.data$low_up), alpha=alpha, color=NA, fill="blue") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$up_low, ymax=.data$up_up), alpha=alpha, color=NA, fill="blue") +
    ggplot2::ylab("")
  
  return(plot)
}
