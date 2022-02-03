#_______________________________________________________________________________
#----                       event_iteration class                           ----
#_______________________________________________________________________________

checkEventIteration <- function(object) {
  return(expectOneForAll(object, c("start", "end", "index", "maxIndex")))
}

setClass(
  "event_iteration",
  representation(
    index = "integer",
    start = "numeric",
    end = "numeric",
    inits = "data.frame",
    maxIndex = "integer"
  ),
  prototype=prototype(inits=data.frame()),
  validity=checkEventIteration
)

#' 
#' Create an event iteration object.
#' 
#' @param index iteration index, starts at 1
#' @param start iteration start time
#' @param end iteration end time
#' @param inits initial values for all subjects, data frame
#' @param maxIndex the last iteration index
#' @return an event iteration object
#' @keywords internal
EventIteration <- function(index, start, end, inits=data.frame(), maxIndex) {
  return(new("event_iteration", index=index, start=start, end=end, inits=inits, maxIndex=maxIndex))
}

#' Get list of event iterations.
#' 
#' @param events events
#' @param maxTime simulation max time
#' @return a list of event iterations
#' @keywords internal
#'
getEventIterations <- function(events, maxTime) {
  userEventTimes <- events %>% getTimes()
  eventTimes <- userEventTimes %>% append(c(0, maxTime)) %>% unique() %>% base::sort()
  if (0 %in% userEventTimes || maxTime==0) {
    # Add 'second' zero at the beginning of time vector
    eventTimes <- eventTimes %>% append(0, after=0)
  }
  from <- eventTimes[-length(eventTimes)]
  to <- eventTimes[-1]
  maxIndex <- to %>% length()
  retValue <- list()
  for (index in seq_along(from)) {
    .x <- from[index]
    .y <- to[index]
    retValue <- retValue %>% append(EventIteration(index=index, start=.x, end=.y, maxIndex=maxIndex))
  }
  return(retValue)
}

#' Cut table according to given iteration.
#' 
#' @param table whole table, data frame
#' @param iteration current iteration being processed
#' @param summary dataset summary
#' @importFrom dplyr across all_of group_by left_join mutate row_number ungroup
#' @keywords internal
#' 
cutTableForEvent <- function(table, iteration, summary) {
  start <- iteration@start
  end <- iteration@end
  inits <- iteration@inits
  
  # Filter on iteration
  table_ <- table %>% dplyr::filter((.data$EVID==1 & .data$TIME >= start & .data$TIME < end) |
                                    (.data$EVID==0 & .data$TIME >= start & .data$TIME <= end))

  # First observation should always be EVENT_RELATED
  # Except for first iteration
  if (start > 0) {
    table_ <- table_ %>% dplyr::group_by(dplyr::across("ID")) %>%
      dplyr::mutate(EVENT_RELATED=ifelse(dplyr::row_number()==1, as.integer(TRUE), .data$EVENT_RELATED))
  }
  
  # Update time-varying covariates
  vars <- summary@event_covariate_names
  if (vars %>% length() > 0 && inits %>% nrow() > 0) {
    update <- inits %>% dplyr::select(dplyr::all_of(c("ID", vars)))
    # Remove old values and left join new values
    table_ <- table_ %>% dplyr::select(-dplyr::all_of(vars)) %>% dplyr::left_join(update, by="ID")
  }
  # Substract starting time to start at 0
  table_$TIME <- table_$TIME - start
  
  return(table_ %>% dplyr::ungroup())
}
