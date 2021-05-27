#_______________________________________________________________________________
#----                       event_iteration class                           ----
#_______________________________________________________________________________

checkEventIteration <- function(object) {
  return(expectOneForAll(object, c("start", "end", "multiple")))
}

setClass(
  "event_iteration",
  representation(
    start = "numeric",
    end = "numeric",
    inits = "data.frame",
    multiple = "logical"
  ),
  prototype=prototype(inits=data.frame(), multiple=FALSE),
  validity=checkEventIteration
)

#' 
#' Create an event iteration object.
#' 
#' @param start iteration start time
#' @param end iteration end time
#' @param inits initial values for all subjects, data frame
#' @param multiple TRUE if multiple iterations (i.e. simulation needs to stopped at least once), FALSE otherwise
#' @return an event iteration object
#' @keywords internal
EventIteration <- function(start, end, inits=data.frame(), multiple=FALSE) {
  return(new("event_iteration", start=start, end=end, inits=inits, multiple=multiple))
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
  multiple <- userEventTimes %>% length() > 0
  eventTimes <- userEventTimes %>% append(c(0, maxTime)) %>% unique() %>% base::sort()
  if (0 %in% userEventTimes || maxTime==0) {
    # Add 'second' zero at the beginning of time vector
    eventTimes <- eventTimes %>% append(0, after=0)
  }
  retValue <- purrr::map2(eventTimes[-length(eventTimes)], eventTimes[-1], .f=function(.x, .y){
    return(EventIteration(start=.x, end=.y, multiple=multiple))
  })
  return(retValue)
}

#' Cut table according to given iteration.
#' 
#' @param table whole table, data frame
#' @param iteration current iteration being processed
#' @param summary dataset summary
#' @keywords internal
#' 
cutTableForEvent <- function(table, iteration, summary) {
  start <- iteration@start
  end <- iteration@end
  inits <- iteration@inits
  
  # Filter on iteration
  table_ <- table %>% dplyr::filter((EVID==1 & TIME >= start & TIME < end) |
                                      (EVID==0 & TIME >= start & TIME <= end))

  # First observation should always be EVENT_RELATED
  # Except for first iteration
  if (start > 0) {
    table_ <- table_ %>% dplyr::group_by(ID) %>% dplyr::mutate(EVENT_RELATED=ifelse(dplyr::row_number()==1, as.integer(TRUE), EVENT_RELATED))
  }
  
  # Update time-varying covariates
  vars <- summary@time_varying_covariate_names
  if (vars %>% length() > 0 && inits %>% nrow() > 0) {
    update <- inits %>% dplyr::select(dplyr::all_of(c("id", vars))) %>% dplyr::rename(ID=id)
    # Remove old values and left join new values
    table_ <- table_ %>% dplyr::select(-dplyr::all_of(vars)) %>% dplyr::left_join(update, by="ID")
  }
  # Substract starting time to start at 0
  table_$TIME <- table_$TIME - start
  
  return(table_)
}