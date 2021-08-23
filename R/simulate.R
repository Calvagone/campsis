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
#' @param nocb next-observation carried backward mode (NOCB), default value is TRUE for mrgsolve, FALSE for RxODE
#' @param ... optional arguments like declare
#' @return dataframe with all results
#' @export
#' @rdname simulate
simulate <- function(model, dataset, dest=NULL, events=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, nocb=NULL, ...) {
  stop("No default function is provided")
}

setGeneric("simulate", function(model, dataset, dest=NULL, events=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, nocb=NULL, ...) {
  if (is.null(dest)) {
    # Default engine
    dest <- "RxODE"
  }
  events <- preprocessEvents(events)
  tablefun <- preprocessFunction(tablefun, "tablefun")
  outvars <- preprocessOutvars(outvars)
  outfun <- preprocessFunction(outfun, "outfun")
  seed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates)
  nocb <- preprocessNocb(nocb, dest)
  standardGeneric("simulate")
})

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
exportTableDelegate <- function(model, dataset, dest, events, seed, tablefun, nocb, nocbvars) {
  if (is(dataset, "dataset")) {
    # Retrieve event times (same for all arms)
    eventTimes <- c(0, events %>% getTimes()) %>% unique()
    
    # Add all 'event-related' times in each arm
    for (armIndex in seq_len(dataset@arms %>% length())) {
      arm <- dataset@arms@list[[armIndex]]
      obsTimes <- arm %>% getTimes()
      if (obsTimes %>% length()==0) {
        stop(paste0("Arm ", arm@id , " does not contain any observation."))
      }
      eventRelatedTimes <- eventTimes[!(eventTimes %in% obsTimes)]
      if (eventRelatedTimes %>% length() > 0) {
        eventRelatedObs <- EventRelatedObservations(times=eventRelatedTimes, compartment=NA)
        dataset@arms@list[[armIndex]] <- dataset@arms@list[[armIndex]] %>% add(eventRelatedObs)
      }
    }
    table <- dataset %>% campsismod::export(dest=dest, model=model, seed=seed, nocb=nocb, event_related_column=TRUE, nocbvars=nocbvars)
  } else {
    table <- dataset
    if (!("EVENT_RELATED" %in% colnames(table))) {
      table <- table %>% dplyr::mutate(EVENT_RELATED=as.integer(FALSE))
    }
  }
  table <- tablefun(table)
  return(table)
}

#' Get dataset max time.
#' 
#' @param dataset dataset
#' @return max time of dataset, whatever its form, 2-dimensional or structured
#' @keywords internal
#' 
getDatasetMaxTime <- function(dataset) {
  if (is(dataset, "dataset")) {
    times <- dataset %>% getTimes()
  } else {
    times <- dataset$TIME
  }
  if (is.null(times) || times %>% length()==0) {
    stop(paste0("Dataset does not contain any observation."))
  }
  return(max(times))
}

#' Simulation delegate core (single replicate).
#' 
#' @inheritParams simulate
#' @param replicate current replicate number
#' @param iterations number of iterations
#' @keywords internal
#' 
simulateDelegateCore <- function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb=nocb, replicate, iterations, ...) {
  destEngine <- getSimulationEngineType(dest)
  tableSeed <- getSeedForDatasetExport(seed=seed, replicate=replicate, iterations=iterations %>% length())
  nocbvars <- processExtraArg(list(...), name="nocbvars", default=NULL, mandatory=FALSE)
  table <- exportTableDelegate(model=model, dataset=dataset, dest=dest, events=events, seed=tableSeed, tablefun=tablefun, nocb=nocb, nocbvars=nocbvars)
  summary <- processExtraArg(list(...), name="summary", default=DatasetSummary(), mandatory=TRUE)
  
  inits <- data.frame()
  results <- NULL
  for (iteration in iterations) {
    iteration@inits <- inits
    table_ <- cutTableForEvent(table, iteration, summary)
    #print(table_)
    results_ <- simulate(model=model, dataset=table_, dest=destEngine, events=events, tablefun=tablefun,
                         outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, replicate=replicate, iteration=iteration, ...)
    # Shift times back to their original value
    results_$time <- results_$time + iteration@start
    
    # Store initial values for next iteration
    inits <- results_ %>% dplyr::group_by(id) %>% dplyr::slice(which.max(time))
    
    # Set seed for next simulation
    iterationSeed <- getSeedForIteration(seed=seed, replicate=replicate, iterations=iterations %>% length(), iteration=iteration@index)
    setSeed(iterationSeed)

    # Calling events
    for (event in events@list) {
      if (iteration@end %in% event@times) {
        inits <- event@fun(inits)
      }
    }
    
    # Get rid of event related observations and remove column
    
    results_ <- results_ %>% dplyr::filter(EVENT_RELATED==0) %>% dplyr::select(-EVENT_RELATED)
    
    # Append simulation results to global results
    # Except for iteration 1 from 0 to 0 which is a special case
    if (!(iteration@index==1 && iteration@start==0 && iteration@end==0 && iteration@maxIndex > 1)) {
      results <- results %>% dplyr::bind_rows(results_ %>% dplyr::ungroup())
    }
  }
  # Reorder results dataframe if at least 1 interruption in order to group results by ID
  # Otherwise, the dataframe is already ordered
  if (iterations %>% length() > 0) {
    results <- results %>% dplyr::arrange(id)
  }
  return(outfun(results))
}

#' Process arm labels. Arm identifiers in ARM column are replaced by arm labels
#' as soon as one arm label is provided.
#' 
#' @param campsis CAMPSIS output
#' @param arms all treatment arms
#' @return updated CAMPSIS output with arm labels instead of arm identifiers
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr map_int
#' @importFrom plyr mapvalues
#' 
processArmLabels <- function(campsis, arms) {
  armIds <- arms@list %>% purrr::map_int(~.x@id)
  armLabels <- arms@list %>% purrr::map_chr(~.x@label)
  if (any(!is.na(armLabels))) {
    armLabels <- ifelse(is.na(armLabels), paste("ARM", armIds), armLabels)
    campsis <- campsis %>% dplyr::mutate(ARM=plyr::mapvalues(ARM, from=armIds, to=armLabels))
  }
  return(campsis)
}

#' Simulation delegate (several replicates).
#' 
#' @inheritParams simulate
#' @keywords internal
#' 
simulateDelegate <- function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb=nocb, ...) {
  validObject(model)
  maxTime <- getDatasetMaxTime(dataset)
  iterations <- getEventIterations(events, maxTime=maxTime)
  
  if (replicates==1) {
    return(simulateDelegateCore(model=model, dataset=dataset, dest=dest, events=events,
                                tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, replicate=1, iterations=iterations, ...))
  } else {
    # Get as many models as replicates
    parameterSamplingSeed <- getSeedForParametersSampling(seed=seed)
    setSeed(parameterSamplingSeed)
    models <- model %>% sample(replicates)
    
    # Run all models
    return(purrr::map2_df(.x=models, .y=seq_along(models), .f=function(model_, replicate) {
      return(simulateDelegateCore(model=model_, dataset=dataset, dest=dest, events=events,
                                  tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, replicate=replicate, iterations=iterations, ...))
    }, .id="replicate") %>% dplyr::mutate(replicate=as.integer(replicate)))
  }
}

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "dataset", "character", "events", "function", "character", "function", "integer", "integer", "logical"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb, ...) {
  campsis <- simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, tablefun=tablefun,
                               outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb,
                               summary=toDatasetSummary(dataset), ...)
  return(processArmLabels(campsis, dataset@arms))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "character", "events", "function", "character", "function", "integer", "integer", "logical"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb, ...) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, tablefun=tablefun, 
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, ...))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "data.frame", "character", "events", "function", "character", "function", "integer", "integer", "logical"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb, ...) {
  return(simulateDelegate(model=model, dataset=tibble::as_tibble(dataset), dest=dest, events=events, tablefun=tablefun, 
                                    outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, ...))
})

#' Remove initial conditions.
#' 
#' @param model PMX model
#' @return same model without initial conditions
#' @importFrom purrr keep
#' @keywords internal
#' 
removeInitialConditions <- function(model) {
  properties <- model@compartments@properties@list
  properties_ <- properties %>% purrr::keep(~!is(.x, "compartment_initial_condition"))
  model@compartments@properties@list <- properties_
  return(model)
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
processSimulateArguments <- function(model, dataset, dest, outvars, ...) {
  # Check extra arguments
  args <- list(...)
  iteration <- args$iteration

  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Slice number, set to 6, as the default number of threads in RxoDE
  # This number must correspond to the number of cores available
  # This should be set as static information (not in simulate method)
  # Note that mrgsolve does not seem to have parallelisation (to check)
  # It may also be interesting to parallelise the replicates (see later on)
  if (iteration@maxIndex > 1) {
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
  summary <- processExtraArg(args, name="summary", default=DatasetSummary(), mandatory=TRUE)
  declare <- unique(c(summary@iov_names, summary@covariate_names, summary@occ_names, user_declare, "ARM", "EVENT_RELATED"))    

  # Remove initial conditions from PMX model before export (if present)
  if (iteration@index > 1) {
    model <- removeInitialConditions(model)
  }
  
  # Export PMX model
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>% campsismod::export(dest="RxODE")
  } else if (is(dest, "mrgsolve_engine")) {
    outvars_ <- outvars[!(outvars %in% dropOthers())]
    outvars_ <- unique(c(outvars_, "ARM", "EVENT_RELATED"))
    engineModel <- model %>% campsismod::export(dest="mrgsolve", outvars=outvars_)
  }
  
  # Compute all slice rounds to perform
  sliceRounds <- list(start=seq(1, maxID, by=slices), end=seq(0, maxID-1, by=slices) + slices)
  
  # Prepare all subdatasets (1 event dataframe per slice/round)
  subdatasets <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
    subdataset <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
    return(subdataset)
  })
  
  # Compartment names
  cmtNames <- model@compartments %>% campsismod::getNames()
  
  return(list(declare=declare, engineModel=engineModel, subdatasets=subdatasets,
              dropOthers=dropOthers, iteration=iteration, cmtNames=cmtNames))
}

#' Get initial conditions at simulation start-up.
#' 
#' @param subdataset subdataset to simulate
#' @param iteration current iteration
#' @param cmtNames compartment names
#' @return named numeric vector with the new initial conditions
#' @keywords internal
#' 
getInitialConditions <- function(subdataset, iteration, cmtNames) {
  # Current ID is of length 1 or 6
  currentID <- unique(subdataset$ID) %>% as.integer()
  if (iteration@inits %>% nrow() == 0) {
    inits <- NULL
  } else {
    assertthat::assert_that(currentID %>% length()==1, msg=paste0("Not a single ID: ", paste0(currentID, collapse=",")))
    inits <- iteration@inits %>% dplyr::filter(id==currentID) %>% unlist()
    inits <- inits[cmtNames]
  }
  return(inits)
}

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "rxode_engine", "events", "function", "character", "function", "integer", "integer", "logical"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb, ...) {
  
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  summary <- processExtraArg(list(...), name="summary", default=DatasetSummary(), mandatory=TRUE)
  
  # Retrieve simulation config
  config <- processSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, ...)

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
  
  # Prepare simulation
  keep <- outvars[outvars %in% c(summary@covariate_names, summary@iov_names, colnames(rxmod@omega))]

  results <- config$subdatasets %>% purrr::map_df(.f=function(subdataset) {
    inits <- getInitialConditions(subdataset, iteration=config$iteration, cmtNames=config$cmtNames)

    # Covariate interpolation
    covs_interpolation <- ifelse(nocb, "nocb", "constant")
    
    # Launch simulation with RxODE
    tmp <- RxODE::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=subdataset, returnType="tibble",
                          keep=keep, inits=inits, covs_interpolation=covs_interpolation)
    
    # RxODE does not add the 'id' column if only 1 subject
    if (!("id" %in% colnames(tmp))) {
      tmp <- tmp %>% tibble::add_column(id=unique(subdataset$ID), .before=1)
    }
    return(processDropOthers(tmp, outvars=outvars, dropOthers=config$dropOthers))
  })
  return(results)
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "mrgsolve_engine", "events", "function", "character", "function", "integer", "integer", "logical"),
          definition=function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb, ...) {
  
  # Retrieve simulation config
  config <- processSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, ...)
  
  # Export PMX model to RxODE
  mrgmod <- config$engineModel
  
  # Disable IIV in mrgsolve model
  mrgmod@omega <- character(0) # IIV managed by pmxsim
  
  # Declare all ETA's in the PARAM block
  omegas <- rxodeMatrix(model, type="omega")
  for (omega in (model@parameters %>% campsismod::select("omega"))@list) {
    if(omega %>% isDiag()) {
      etaName <- omega %>% campsismod::getNameInModel()
      mrgmod@param <- mrgmod@param %>% append(paste0(etaName, " : ", 0, " : ", etaName))
    }
  }
  # Declare all covariates and IOV variables contained in dataset
  # Also declare ARM and user-input variables in 'declare' extra arg
  for (variable in config$declare) {
    mrgmod@param <- mrgmod@param %>% append(paste0(variable, " : ", 0, " : ", variable))
  }
  
  # Instantiate mrgsolve model
  mod <- mrgsolve::mcode("model", mrgmod %>% campsismod::toString())
  
  results <- config$subdatasets %>% purrr::map_df(.f=function(subdataset) {
    inits <- getInitialConditions(subdataset, iteration=config$iteration, cmtNames=config$cmtNames)

    # Update init vector (see mrgsolve script: 'update.R')
    if (!is.null(inits)) {
      mod <- mod %>% mrgsolve::update(init=inits)
    }
    
    # Launch simulation with mrgsolve
    # Observation only set to TRUE to align results with RxODE
    tmp <- mod %>% mrgsolve::data_set(data=subdataset) %>% mrgsolve::mrgsim(obsonly=TRUE, output="df", nocb=nocb) %>% tibble::as_tibble()
    
    # Use same id and time columns as RxODE
    tmp <- tmp %>% dplyr::rename(id=ID, time=TIME)
    return(processDropOthers(tmp, outvars=outvars, dropOthers=config$dropOthers))
  })
  return(results)
})
