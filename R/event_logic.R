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
  if (0 %in% userEventTimes) {
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
  
  table_ <- table %>% dplyr::filter((EVID==1 & TIME >= start & TIME < end) |
                                      (EVID==0 & TIME > start & TIME <= end) |
                                      (EVID==0 & start==0 & TIME==0))
  # All the current information is not related to events
  table_ <- table_ %>% dplyr::mutate(EVENT_RELATED=as.numeric(0))
  
  # Make sure there is an starting and an ending observation
  table_ <- table_ %>% dplyr::group_by(ID) %>% dplyr::group_modify(.f=function(x, y) {
    if (x %>% dplyr::filter(EVID==0 & TIME==start) %>% nrow() == 0) {
      # Copy first row
      # Please note this first observation is only 'useful' to mrgsolve
      # If no observation at time 'start' (i.e. 0 after time substraction), initial conditions will apply to first observation in dataset...
      # While in RxODE, simulation/initial conditions always start at 0
      firstRowCopy <- x %>% dplyr::slice(which.min(TIME))
      firstRowCopy <- firstRowCopy %>% dplyr::mutate(TIME=start, EVID=as.integer(0), MDV=as.integer(0), AMT=as.numeric(NA),
                                                     CMT=as.integer(1), RATE=as.numeric(0), DOSENO=as.integer(NA),
                                                     EVENT_RELATED=as.numeric(1))
      x <- firstRowCopy %>% dplyr::bind_rows(x)
    }
    if (x %>% dplyr::filter(EVID==0 & TIME==end) %>% nrow() == 0) {
      # Copy last row
      lastRowCopy <- x %>% dplyr::slice(which.max(TIME))
      lastRowCopy <- lastRowCopy %>% dplyr::mutate(TIME=end, EVID=as.integer(0), MDV=as.integer(0), AMT=as.numeric(NA),
                                                   CMT=as.integer(1), RATE=as.numeric(0), DOSENO=as.integer(NA),
                                                   EVENT_RELATED=as.numeric(1))
      x <- x %>% dplyr::bind_rows(lastRowCopy)
    }
    return(x)
  })
  
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