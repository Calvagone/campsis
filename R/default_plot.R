
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
#' @param variable variable to show
#' @param colour variable(s) to colour
#' @return plot
#' @importFrom ggplot2 aes ggplot geom_line
#' @export
spaghettiPlot <- function(x, variable, colour=NULL) {
  group <- "GROUP_GGPLOT"
  x <- uniteColumns(x=x %>% obsOnly(), columns=c("ID", colour), colname=group)
  
  if (length(colour) > 0) {
    colourColumn <- "COLOUR_GGPLOT"
    x <- uniteColumns(x=x, columns=colour, colname=colourColumn)
  } else {
    colourColumn <- NULL
  }
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=.data$TIME, y=.data[[variable]], group=.data[[group]], colour=getColumn(.data, colourColumn))) +
    ggplot2::geom_line()
  
  if (length(colour) > 0) {
    plot <- plot + ggplot2::labs(colour=paste0(colour, collapse = " / "))
  }
    
  return(plot)
}

#' Shaded plot (or prediction interval plot).
#' 
#' @param x data frame
#' @param variable variable to show
#' @param colour variable(s) to colour
#' @param strat_extra variable(s) to stratify, but not to colour (useful for use with facet_wrap)
#' @param level PI level, default is 0.9 (90\% PI)
#' @param alpha alpha parameter (transparency) given to geom_ribbon
#' @return a ggplot object
#' @importFrom ggplot2 aes ggplot geom_line geom_ribbon ylab
#' @export
shadedPlot <- function(x, variable, colour=NULL, strat_extra=NULL, level=0.90, alpha=0.25) {
  if (length(colour) > 0) {
    colourColumn <- "COLOUR_GGPLOT"
    x <- uniteColumns(x=x %>% obsOnly(), columns=colour, colname=colourColumn)
  } else {
    colourColumn <- NULL
  }
  strata_names <- c(colour, strat_extra, colourColumn)
  strata <- if (is.null(strata_names)) NULL else setNames(rep(allStrataLevels(), length(strata_names)), strata_names)

  x <- PI(x=x, variable=variable, strata=strata, level=level, gather=FALSE)

  plot <- ggplot2::ggplot(data=x, mapping=ggplot2::aes(x=.data$TIME, colour=getColumn(.data, colourColumn))) +
    ggplot2::geom_line(ggplot2::aes(y=.data$med)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$low, ymax=.data$up, colour=getColumn(.data, colourColumn), fill=getColumn(.data, colourColumn)), colour=NA, alpha=alpha) +
    ggplot2::ylab(variable)
  
  if (length(colour) > 0) {
    plot <- plot + ggplot2::labs(colour=paste0(colour, collapse = " / "),
                                 fill=paste0(colour, collapse = " / "))
  }

  return(plot)
}

#' Scatter plot (or X vs Y plot).
#' 
#' @param x data frame
#' @param output the 2 variables to show, character vector
#' @param colour variable(s) to colour
#' @param time the time to look at those 2 variables, if NULL, min time is used (usually 0)
#' @return a ggplot object
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes ggplot geom_point
#' @export
scatterPlot <- function (x, variable, colour=NULL, time=NULL) {
  group <- "GROUP_GGPLOT"
  x <- uniteColumns(x=x %>% obsOnly(), columns=c("ID", colour), colname=group)
  
  if (is.null(time)) {
    time <- min(x$TIME)
  }
  x <- x %>% dplyr::filter(.data$TIME %in% time)
  
  if (variable %>% length() == 1) {
    x$MY_VARIABLE_2 <- 0
    variable <- c(variable, "MY_VARIABLE_2")
  } else if (variable %>% length() > 2) {
    stop("Please provide 2 variables at most !")
  }

  if (length(colour) > 0) {
    colourColumn <- "COLOUR_GGPLOT"
    x <- uniteColumns(x=x, columns=colour, colname=colourColumn)
  } else {
    colourColumn <- NULL
  }
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=.data[[variable[1]]], y=.data[[variable[2]]], group=.data[[group]], colour=getColumn(.data, colourColumn))) +
    ggplot2::geom_point()
  
  if (length(colour) > 0) {
    plot <- plot + ggplot2::labs(colour=paste0(colour, collapse = " / "))
  }
  
  return(plot)
}

#' VPC plot.
#' 
#' @param x data frame, output of CAMPSIS with replicates
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all").
#'   Only columns that are actually present in \code{x} are used.
#' @param level PI level, default is 0.9 (90\% PI)
#' @param alpha alpha parameter (transparency) given to geom_ribbon
#' @return a ggplot object
#' @importFrom ggplot2 aes ggplot ylab
#' @export
vpcPlot <- function(x, strata=NULL, level=0.90, alpha=0.15) {
  if (length(strata) > 1) {
    stop("Currently max 1 stratification variable is allowed")
  }
  summary <- VPC(x=x, strata=strata, level=level)
  if (length(strata) > 0) {
    group <- "GROUP_GGPLOT"
    summary <- uniteColumns(x=summary, columns=names(strata), colname=group)
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
