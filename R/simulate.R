#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

#' Simulate function.
#' 
#' @param model generic PMX model
#' @param dataset PMX dataset or 2-dimensional table
#' @param dest destination simulation engine, default is 'RxODE'
#' @param events interruption events
#' @param tablefun function or lambda formula to apply on exported 2-dimensional dataset
#' @param outvars variables to output in resulting dataframe
#' @param outfun function or lambda formula to apply on resulting dataframe after each replicate
#' @param seed seed value
#' @param replicates number of replicates, default is 1
#' @param ... optional arguments like declare
#' @return dataframe with all results
#' @export
simulate <- function(model, dataset, dest=NULL, events=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, ...) {
  stop("No default function is provided")
}

setGeneric("simulate", function(model, dataset, dest=NULL, events=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, ...) {
  dest <- if (is.null(dest)) "RxODE" else dest
  events <- preprocessEvents(events)
  tablefun <- preprocessFunction(tablefun, "tablefun")
  outvars <- preprocessOutvars(outvars)
  outfun <- preprocessFunction(outfun, "outfun")
  seed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates)
  standardGeneric("simulate")
})

#' Pre-process events.
#'
#' @param events interruption events
#' @keywords internal
#' 
preprocessEvents <- function(events) {
  if (is.null(events)) {
    return(Events())
  } else {
    return(events)
  }
}

#' Pre-process function argument.
#'
#' @param fun function or lambda formula
#' @param name function name
#' @return a function in any case
#' @importFrom assertthat assert_that
#' @importFrom plyr is.formula
#' @importFrom rlang as_function
#' @keywords internal
#' 
preprocessFunction <- function(fun, name) {
  if (is.null(fun)) {
    fun <- function(x){x}
    return(fun)
  } else {
    assertthat::assert_that(is.function(fun) || plyr::is.formula(fun),
                            msg=paste0(name, " must be a function or a lambda formula"))
    if (plyr::is.formula(fun)) {
      fun <- rlang::as_function(fun)
      # Class of fun is c("rlang_lambda_function","function")
      # However, not accepted as argument if method signature is "function"... Bug?
      # Workaround is to set a unique class
      class(fun) <- "function"
    }
    return(fun)
  }
}

#' Preprocess 'outvars' argument. Outvars is a character vector which tells
#' pmxsim the mandatory columns to keep in the output dataframe.
#'
#' @param outvars character vector or function
#' @return outvars
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessOutvars <- function(outvars) {
  if (is.null(outvars)) {
    return(character(0))
  } else {
    assertthat::assert_that(is.character(outvars), 
                            msg="outvars must be a character vector with the column names to keep")
    
    # In any cases, we should never see these special variables
    outvars <- outvars[!(outvars %in% c("id", "time", "ARM"))]
    return(outvars)
  }
}

#' Preprocess 'replicates' argument.
#' 
#' @param replicates number of replicates
#' @return same number, but as integer
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessReplicates <- function(replicates) {
  assertthat::assert_that(is.numeric(replicates) && replicates%%1==0 && replicates > 0,
                          msg="replicates not a positive integer")
  return(as.integer(replicates))
}

#' Get simulation engine type.
#' 
#' @param dest destination engine, string form
#' @return simulation engine type
#' @keywords internal
#' 
getSimulationEngineType <- function(dest) {
  if (dest=="RxODE") {
    engine <- new("rxode_engine")
  } else if (dest=="mrgsolve") {
    engine <- new("mrgsolve_engine")
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
  return(engine)
}

#' Export table delegate.
#' 
#' @inheritParams simulate
#' @keywords internal
#' 
exportTableDelegate <- function(model, dataset, dest, events, seed, tablefun) {
  if (is(dataset, "dataset")) {
    table <- dataset %>% export(dest=dest, model=model, seed=seed)
  } else {
    table <- dataset
  }
  table <- tablefun(table)
  return(table)
}

#' Cut table for event starting at 'start' and ending at 'end'.
#' 
#' @param table whole table, data frame
#' @param current_event current event being processed
#' @keywords internal
#' 
cutTableForEvent <- function(table, current_event) {
  start <- current_event$start
  end <- current_event$end
  table_ <- table %>% dplyr::filter((EVID==1 & TIME >= start & TIME < end) |
                                    (EVID==0 & TIME > start & TIME <= end) |
                                    (EVID==0 & start==0 & TIME==0))
  # All the current information is not related to events
  table_ <- table_ %>% dplyr::mutate(EVENT_RELATED=as.numeric(0))
  
  # Make sure there is an ending observation
  table_ <- table_ %>% dplyr::group_by(ID) %>% dplyr::group_modify(.f=function(x, y) {
    if (x %>% dplyr::filter(EVID==0 & TIME==start) %>% nrow() == 0) {
      # Copy first row
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
  
  # Substract starting time to start at 0
  table_$TIME <- table_$TIME - start
  
  return(table_)
}

#' Get list of event times.
#' 
#' @param events events
#' @param maxTime simulation max time
#' @return a list of numeric lists with names 'start' and 'end'
#' @keywords internal
#'
getEventTimes <- function(events, maxTime) {
  eventTimes <- events %>% getTimes()
  eventTimes <- eventTimes %>% append(c(0, maxTime)) %>% unique() %>% base::sort()
  # Transform into list
  eventTimes_ <- purrr::map2(eventTimes[-length(eventTimes)], eventTimes[-1], .f=function(.x, .y){
    return(list(start=.x, end=.y))
  })
  return(eventTimes_)
}
  
#' Simulation delegate.
#' 
#' @inheritParams simulate
#' @keywords internal
#' 
simulateDelegate <- function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, ...) {
  validObject(model)
  destEngine <- getSimulationEngineType(dest)
  
  if (replicates==1) {
    table <- exportTableDelegate(model=model, dataset=dataset, dest=dest, events=events, seed=seed, tablefun=tablefun)
    eventTimes <- getEventTimes(events, max(table$TIME))
    inits <- NULL
    results <- NULL
    for (eventIndex in seq_along(eventTimes)) {
      current_event <- eventTimes[[eventIndex]]
      current_event$inits <- inits
      current_event$multiple_events <- eventTimes %>% length() > 1
      current_event$cmtNames <- model@compartments %>% getNames()
      table_ <- cutTableForEvent(table, current_event)
      results_ <- simulate(model=model, dataset=table_, dest=destEngine, events=events, tablefun=tablefun,
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, current_event=current_event, ...)
      # Shift times back to their original value
      results_$time <- results_$time + current_event$start
      
      # Store initial values for next iteration
      inits <- results_ %>% dplyr::group_by(id) %>% dplyr::slice(which.max(time))
      
      # Get rid of event related observations and remove column
      results_ <- results_ %>% dplyr::filter(EVENT_RELATED==0) %>% dplyr::select(-EVENT_RELATED)
      
      # Append simulation results to global results
      results <- results %>% dplyr::bind_rows(results_)
      
    }
    return(results)
  } else {
    # Get as many models as replicates
    setSeed(seed - 1) # Set seed before sampling parameters uncertainty
    models <- model %>% sample(replicates)
    
    # Run all models
    return(purrr::map2_df(.x=models, .y=seq_along(models), .f=function(model_, replicate) {
      seedInRep <- getSeedForReplicate(seed, replicate)
      table <- exportTableDelegate(model=model_, dataset=dataset, dest=dest, events=events, seed=seedInRep, tablefun=tablefun)
      # TMP
      current_event <- list()
      current_event$inits <- NULL
      current_event$multiple_events <- FALSE
      return(simulate(model=model_, dataset=table, dest=destEngine, events=events, tablefun=tablefun,
                      outvars=outvars, outfun=outfun, seed=seedInRep, replicates=replicates, current_event=current_event, ...))
    }, .id="replicate"))
  }
}

setMethod("simulate", signature=c("pmx_model", "dataset", "character", "events", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, ...) {
  iovNames <- dataset %>% getIOVNames()
  covariateNames <- dataset %>% getCovariateNames()
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, tablefun=tablefun,
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                          iovNames=iovNames, covariateNames=covariateNames, ...))
})


setMethod("simulate", signature=c("pmx_model", "data.frame", "character", "events", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, ...) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, tablefun=tablefun, 
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, ...))
})

#' Preprocess subjects ID's.
#' 
#' @param dataset current dataset, data frame form
#' @return list of consecutive ID's
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessIds <- function(dataset) {
  ids <- unique(dataset$ID)
  maxID <- max(ids)
  assertthat::assert_that(all(ids==seq_len(maxID)), msg="ID's must be consecutive numbers, starting at 1")
  return(ids)
}

#' Preprocess ARM column. Add ARM equation in model automatically.
#' 
#' @param dataset current dataset, data frame form
#' @param model model
#' @return updated model
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessArmColumn <- function(dataset, model) {
  if ("ARM" %in% colnames(dataset)) {
    pkRecord <- model@model %>% pmxmod::getByName("MAIN")
    pkRecord@code <- c(pkRecord@code, "ARM=ARM")
    model@model <- model@model %>% pmxmod::replace(pkRecord)
  }
  if ("EVENT_RELATED" %in% colnames(dataset)) {
    pkRecord <- model@model %>% pmxmod::getByName("MAIN")
    pkRecord@code <- c(pkRecord@code, "EVENT_RELATED=EVENT_RELATED")
    model@model <- model@model %>% pmxmod::replace(pkRecord)
  }
  return(model)
}

#' Preprocess 'slices' argument.
#' 
#' @param slices slices argument corresponding to the number of subjects simulated at once
#' @return slices if not NULL, otherwise total number of subjects
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessSlices <- function(slices, maxID) {
  if (is.null(slices)) {
    return(maxID)
  } else {
    assertthat::assert_that(is.numeric(slices) && slices%%1==0 && slices > 0,
                            msg="slices not a positive integer")
    return(slices)
  }
}

#' Return the 'DROP_OTHERS' string that may be used in the 'outvars' vector for
#' RxODE/mrgsolve to drop all others variables that are usually output in the resulting data frame.
#' 
#' @return a character value
#' @keywords internal
#' 
dropOthers <- function() {
  return("DROP_OTHERS")
}

#' Process 'DROP_OTHERS'.
#'
#' @param x the current data frame
#' @param outvars variables to keep
#' @param dropOthers logical value
#' @return processed data frame
#' @keywords internal
#' 
processDropOthers <- function(x, outvars=character(0), dropOthers) {
  if (!dropOthers) {
    return(x)
  }
  outvars_ <- outvars[!(outvars %in% dropOthers())]
  out <- c("id", "time", "ARM", "EVENT_RELATED", outvars_)
  names <- colnames(x)
  return(x[, names[names %in% out]])
}

#' Preprocess arguments of the simulate method.
#' 
#' @param model PMX model
#' @param dataset dataset, data.frame form
#' @param dest destination engine
#' @param outvars outvars
#' @param ... all other arguments
#' @return a simulation configuration
#' @importFrom purrr map2
#' @keywords internal
#' 
preprocessSimulateArguments <- function(model, dataset, dest, outvars, ...) {
  # Check extra arguments
  args <- list(...)
  currentEvent <- args$current_event

  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Slice number, put to 6, as the default number of threads in RxoDE
  # This number must correspond to the number of cores available
  # This should be set as static information (not in simulate method)
  # Note that mrgsolve does not seem to have parallelisation (to check)
  # It may also be interesting to parallelise the replicates (see later on)
  if (currentEvent$multiple_events) {
    slices <- 1
    #print("Multiple events")
  } else {
    slices <- preprocessSlices(6, maxID=maxID)
    #print("Single event")
  }
  
  # Drop others 'argument'
  dropOthers <- dropOthers() %in% outvars

  # Extra argument declare (for mrgsolve only)
  user_declare <- processExtraArg(args, name="declare", mandatory=FALSE)
  iovNames <- processExtraArg(args, name="iovNames", mandatory=FALSE)
  covariateNames <- processExtraArg(args, name="covariateNames", mandatory=FALSE)
  declare <- unique(c(iovNames, covariateNames, user_declare, "ARM", "EVENT_RELATED"))    

  # Export PMX model
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>% pmxmod::export(dest="RxODE")
  } else if (is(dest, "mrgsolve_engine")) {
    outvars_ <- outvars[!(outvars %in% dropOthers())]
    outvars_ <- unique(c(outvars_, "ARM", "EVENT_RELATED"))
    engineModel <- model %>% pmxmod::export(dest="mrgsolve", outvars=outvars_)
  }
  
  # Compute all slice rounds to perform
  sliceRounds <- list(start=seq(1, maxID, by=slices), end=seq(0, maxID-1, by=slices) + slices)
  
  # Prepare list of events (1 event dataframe per slice/round)
  eventsList <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
    events <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
    return(events)
  })
  
  return(list(declare=declare, engineModel=engineModel, eventsList=eventsList,
              dropOthers=dropOthers, iovNames=iovNames, covariateNames=covariateNames, currentEvent=currentEvent))
}

getInitialConditions <- function(events, currentEvent) {
  eInits <- currentEvent$inits
  eCmt <- currentEvent$cmtNames
  
  # Current ID is of length 1 or 6
  currentID <- unique(events$ID) %>% as.integer()
  if (is.null(eInits)) {
    inits <- NULL
  } else {
    assertthat::assert_that(currentID %>% length()==1, msg="eInits not null, slices must be 1")
    inits <- eInits %>% dplyr::filter(id==currentID) %>% unlist()
    inits <- inits[eCmt]
  }
  return(inits)
}

setMethod("simulate", signature=c("pmx_model", "data.frame", "rxode_engine", "events", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, ...) {
  
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  
  # Retrieve simulation config
  config <- preprocessSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, ...)

  # Instantiate RxODE model
  rxmod <- config$engineModel
  mod <- RxODE::RxODE(paste0(rxmod@code, collapse="\n"))
  
  # Preparing parameters
  params <- rxmod@theta
  sigma <- rxmod@sigma
  if (nrow(sigma)==0) {
    sigma <- NULL
  }
  # Fake OMEGA to avoid RxODE warning if several subjects in dataset
  omega <- matrix(1)
  fakeEtaName <- "FAKE_ETA"
  rownames(omega) <- fakeEtaName
  colnames(omega) <- fakeEtaName
  
  # Launch RxODE
  eventsList <- config$eventsList
  dropOthers <- config$dropOthers
  keep <- outvars[outvars %in% c(config$covariateNames, config$iovNames, colnames(rxmod@omega))]

  results <- eventsList %>% purrr::map_df(.f=function(events) {
    inits <- getInitialConditions(events, config$currentEvent)
    tmp <- RxODE::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=events, returnType="tibble", keep=keep, inits=inits)
    
    # RxODE does not add the 'id' column if only 1 subject
    if (!("id" %in% colnames(tmp))) {
      tmp <- tmp %>% tibble::add_column(id=unique(events$ID), .before=1)
    }
    return(processDropOthers(tmp, outvars=outvars, dropOthers=dropOthers))
  })
  return(outfun(results))
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "mrgsolve_engine", "events", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, ...) {
  
  # Retrieve simulation config
  config <- preprocessSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, ...)
  
  # Export PMX model to RxODE
  mrgmod <- config$engineModel
  
  # Disable IIV in mrgsolve model
  mrgmod@omega <- character(0) # IIV managed by pmxsim
  
  # Declare all ETA's in the PARAM block
  omegas <- rxodeMatrix(model, type="omega")
  for (omega in (model@parameters %>% pmxmod::select("omega"))@list) {
    if(omega %>% isDiag()) {
      etaName <- omega %>% pmxmod::getNameInModel()
      mrgmod@param <- mrgmod@param %>% append(paste0(etaName, " : ", 0, " : ", etaName))
    }
  }
  # Declare all covariates and IOV variables contained in dataset
  # Also declare ARM and user-input variables in 'declare' extra arg
  for (variable in config$declare) {
    mrgmod@param <- mrgmod@param %>% append(paste0(variable, " : ", 0, " : ", variable))
  }
  
  # Instantiate mrgsolve model
  mod <- mrgsolve::mcode("model", mrgmod %>% pmxmod::toString())
  
  # Launch mrgsolve
  eventsList <- config$eventsList
  dropOthers <- config$dropOthers
  
  results <- eventsList %>% purrr::map_df(.f=function(events){
    inits <- getInitialConditions(events, config$currentEvent)

    # Update init vector (see mrgsolve script: 'update.R')
    if (!is.null(inits)) {
      mod <- mod %>% mrgsolve::update(init=inits)
    }
    
    # Observation only set to TRUE to align results with RxODE
    tmp <- mod %>% mrgsolve::data_set(data=events) %>% mrgsolve::mrgsim(obsonly=TRUE, output="df")
    
    # Use same id and time columns as RxODE
    tmp <- tmp %>% dplyr::rename(id=ID, time=TIME)
    return(processDropOthers(tmp, outvars=outvars, dropOthers=dropOthers))
  })
  return(outfun(results))
})
