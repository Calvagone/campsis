#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

#' Simulate function.
#' 
#' @param model generic CAMPSIS model
#' @param dataset CAMPSIS dataset or 2-dimensional table
#' @param dest destination simulation engine, default is 'RxODE'
#' @param events interruption events
#' @param scenarios list of scenarios to be simulated
#' @param tablefun function or lambda formula to apply on exported 2-dimensional dataset
#' @param outvars variables to output in resulting dataframe
#' @param outfun function or lambda formula to apply on resulting dataframe after each replicate
#' @param seed seed value
#' @param replicates number of replicates, default is 1
#' @param nocb next-observation carried backward mode (NOCB), default value is TRUE for mrgsolve, FALSE for RxODE
#' @param dosing output dosing information, default is FALSE
#' @param ... optional arguments like 'declare' and 'nocbvars'
#' @return dataframe with all results
#' @export
#' @rdname simulate
simulate <- function(model, dataset, dest=NULL, events=NULL, scenarios=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, nocb=NULL, dosing=FALSE, ...) {
  stop("No default function is provided")
}

setGeneric("simulate", function(model, dataset, dest=NULL, events=NULL, scenarios=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, nocb=NULL, dosing=FALSE, ...) {
  dest <- preprocessDest(dest)
  events <- preprocessEvents(events)
  scenarios <- preprocessScenarios(scenarios)
  tablefun <- preprocessFunction(tablefun, "tablefun")
  outvars <- preprocessOutvars(outvars)
  outfun <- preprocessFunction(outfun, "outfun")
  seed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates)
  nocb <- preprocessNocb(nocb, dest)
  dosing <- preprocessDosing(dosing)
  
  standardGeneric("simulate")
})

#' Get simulation engine type.
#' 
#' @param dest destination engine, string form
#' @return simulation engine type
#' @keywords internal
#' 
getSimulationEngineType <- function(dest) {
  if (dest=="rxode2") {
    engine <- new("rxode_engine", rxode2=TRUE)
  } else if (dest=="RxODE") {
    engine <- new("rxode_engine", rxode2=FALSE)
  } else if (dest=="mrgsolve") {
    engine <- new("mrgsolve_engine")
  } else {
    stop("Only rxode2, RxODE and mrgsolve are supported for now")
  }
  return(engine)
}

#' Export table delegate.
#' 
#' @inheritParams simulate
#' @return a data frame
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
    table <- dataset %>% export(dest=dest, model=model, seed=seed, nocb=nocb, event_related_column=TRUE, nocbvars=nocbvars)
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
#' @return a data frame with the results
#' @keywords internal
#' @importFrom dplyr across bind_rows group_by slice ungroup
#' 
simulateDelegateCore <- function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, nocb, dosing, replicate, iterations, ...) {
  destEngine <- getSimulationEngineType(dest)
  tableSeed <- getSeedForDatasetExport(seed=seed, replicate=replicate, iterations=iterations %>% length())
  nocbvars <- processExtraArg(list(...), name="nocbvars", default=NULL, mandatory=FALSE)
  table <- exportTableDelegate(model=model, dataset=dataset, dest=dest, events=events, seed=tableSeed, tablefun=tablefun, nocb=nocb, nocbvars=nocbvars)
  summary <- processExtraArg(list(...), name="summary", default=DatasetSummary(), mandatory=TRUE)
  progress <- processExtraArg(list(...), name="progress", default=NULL, mandatory=TRUE)
  progress@iterations <- iterations %>% length()
  
  inits <- data.frame()
  results <- NULL
  for (iteration in iterations) {
    # Update iteration counter
    progress <- progress %>% updateIteration(iteration@index)
    
    iteration@inits <- inits
    table_ <- cutTableForEvent(table, iteration, summary)
    #print(table_)
    results_ <- simulate(model=model, dataset=table_, dest=destEngine, events=events, tablefun=tablefun,
                         outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb,
                         dosing=dosing, replicate=replicate, iteration=iteration, progress=progress, ...)
    # Shift times back to their original value
    results_$TIME <- results_$TIME + iteration@start
    
    # Store initial values for next iteration
    inits <- results_ %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::slice(which.max(.data$TIME))
    
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
    
    results_ <- results_ %>% dplyr::filter(.data$EVENT_RELATED==0) %>% dplyr::select(-dplyr::all_of("EVENT_RELATED"))
    
    # Append simulation results to global results
    # Except for iteration 1 from 0 to 0 which is a special case
    if (!(iteration@index==1 && iteration@start==0 && iteration@end==0 && iteration@maxIndex > 1)) {
      results <- results %>% dplyr::bind_rows(results_ %>% dplyr::ungroup())
    }
  }
  # Reorder results dataframe if at least 1 interruption in order to group results by ID
  # Otherwise, the dataframe is already ordered
  if (iterations %>% length() > 0) {
    results <- results %>% dplyr::arrange(dplyr::across("ID"))
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
#' @keywords internal
#' 
processArmLabels <- function(campsis, arms) {
  armIds <- arms@list %>% purrr::map_int(~.x@id)
  armLabels <- arms@list %>% purrr::map_chr(~.x@label)
  if (any(!is.na(armLabels))) {
    armLabels <- ifelse(is.na(armLabels), paste("ARM", armIds), armLabels)
    campsis <- campsis %>% dplyr::mutate(ARM=plyr::mapvalues(.data$ARM, from=armIds, to=armLabels))
  }
  return(campsis)
}

#' Simulation scenarios.
#' 
#' @inheritParams simulate
#' @return a data frame with the results
#' @keywords internal
#' @importFrom methods validObject
simulateScenarios <- function(scenarios, model, dataset, dest, events,
                              tablefun, outvars, outfun, seed, replicates,
                              nocb, dosing, replicate, progress, ...) {
  outer <-  purrr::map2_df(.x=scenarios@list, .y=seq_along(scenarios@list), .f=function(scenario, scenarioIndex) {
    model <- model %>% applyScenario(scenario)
    dataset <- dataset %>% applyScenario(scenario)
    
    # Validate CAMPSIS model in depth
    methods::validObject(model, complete=TRUE)
    
    # Validate CAMPSIS dataset in depth (btw, validObject also works on non S4 objects)
    methods::validObject(dataset, complete=TRUE)
    
    maxTime <- getDatasetMaxTime(dataset)
    iterations <- getEventIterations(events, maxTime=maxTime)
    
    # Make short summary of dataset
    if (is(dataset, "dataset")) {
      summary <- toDatasetSummary(dataset)
    } else {
      summary <- DatasetSummary() # Default
    }
    
    # Update scenario counter
    progress <- progress %>% updateScenario(scenarioIndex)
    
    inner <- simulateDelegateCore(model=model, dataset=dataset, dest=dest, events=events,
                                    tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                                    nocb=nocb, dosing=dosing, replicate=replicate, iterations=iterations, summary=summary, progress=progress, ...)
    if (scenarios %>% length() > 1) {
      inner <- inner %>% dplyr::mutate(SCENARIO=scenario@name)
    }
    return(inner)
  })
  return(outer)
}

#' Simulation delegate (several replicates).
#' 
#' @inheritParams simulate
#' @return a data frame with the results
#' @keywords internal
#' @importFrom methods validObject
simulateDelegate <- function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, nocb, dosing, ...) {
  # Record progress
  progress <- SimulationProgress(replicates=replicates, scenarios=scenarios %>% length())
  progress@pb$tick(0)
  
  # Single replicate: don't use parameter uncertainty
  if (replicates==1) {
    
    # Update replicate counter
    progress <- progress %>% updateReplicate(1)
    
    retValue <- simulateScenarios(scenarios=scenarios, model=model, dataset=dataset, dest=dest, events=events,
                                  tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                                  nocb=nocb, dosing=dosing, replicate=1, progress=progress, ...)
    # Hide progress bar at the end
    progress@pb$terminate()
    
    return(retValue)
  # More than 1 replicate: parameter uncertainty enabled
  } else {
    # First make sure CAMPSIS model is valid
    methods::validObject(model, complete=TRUE)
    
    # Get as many models as replicates
    parameterSamplingSeed <- getSeedForParametersSampling(seed=seed)
    setSeed(parameterSamplingSeed)
    models <- model %>% sample(replicates)
    
    # Run all models
    allRep <- purrr::map2_df(.x=models, .y=seq_along(models), .f=function(model_, replicate) {
      retValue <- NULL
      
      # Update replicate counter
      progress <- progress %>% updateReplicate(replicate)
      
      tryCatch(
        expr={
          retValue <- simulateScenarios(scenarios=scenarios, model=model_, dataset=dataset, dest=dest, events=events,
                                        tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                                        nocb=nocb, dosing=dosing, replicate=replicate, progress=progress, ...)
          retValue <- dplyr::bind_cols(replicate=replicate, retValue)
        },
        error=function(cond) {
          message(paste0("Error with replicate number ", replicate, ":"))
          print(cond)
        }
      )
      return(retValue)
    })
    
    # Hide progress bar at the end
    progress@pb$terminate()
    
    return(allRep)
  }
}

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "dataset", "character", "events", "scenarios", "function", "character", "function", "integer", "integer", "logical", "logical"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, nocb, dosing, ...) {
  campsis <- simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, scenarios=scenarios, tablefun=tablefun,
                               outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, dosing=dosing, ...)
  return(processArmLabels(campsis, dataset@arms))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "character", "events", "scenarios", "function", "character", "function", "integer", "integer", "logical", "logical"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, nocb, dosing, ...) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, scenarios=scenarios, tablefun=tablefun, 
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, dosing=dosing, ...))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "data.frame", "character", "events", "scenarios", "function", "character", "function", "integer", "integer", "logical", "logical"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, nocb, dosing, ...) {
  return(simulateDelegate(model=model, dataset=tibble::as_tibble(dataset), dest=dest, events=events, scenarios=scenarios, tablefun=tablefun, 
                                    outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, nocb=nocb, dosing=dosing, ...))
})

#' Remove initial conditions.
#' 
#' @param model CAMPSIS model
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
#' @param model CAMPSIS model
#' @param dataset dataset, data.frame form
#' @param dest destination engine
#' @param outvars outvars
#' @param dosing add dosing information, logical value
#' @param ... all other arguments
#' @return a simulation configuration
#' @importFrom purrr map2
#' @keywords internal
#' 
processSimulateArguments <- function(model, dataset, dest, outvars, dosing, ...) {
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
  declare <- unique(c(summary@iov_names, summary@covariate_names, summary@occ_names,
                      summary@tsld_tdos_names, user_declare, "ARM", "EVENT_RELATED"))    

  # Remove initial conditions from CAMPSIS model before export (if present)
  if (iteration@index > 1) {
    model <- removeInitialConditions(model)
  }
  
  
  
  # Export to RxODE / rxode2
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>% export(dest="RxODE")
  
  # Export to mrgsolve
  } else if (is(dest, "mrgsolve_engine")) {
    
    # Export structural model (all THETA's to 0, all OMEGA's to 0)
    structuralModel <- model
    structuralModel@parameters@list <- structuralModel@parameters@list %>% purrr::map(.f=function(parameter) {
      if (is(parameter, "theta") || is(parameter, "omega")) {
        parameter@value <- 0
      }
      return(parameter)
    })
    
    # Set ETA's as extra parameters in mrgsolve
    etaNames <- (model@parameters %>% select("omega"))@list %>%
      purrr::keep(~isDiag(.x)) %>%
      purrr::map_chr(~getNameInModel(.x))
    
    # Extra care to additional outputs which need to be explicitly declared with mrgsolve 
    outvars_ <- outvars[!(outvars %in% dropOthers())]
    outvars_ <- unique(c(outvars_, "ARM", "EVENT_RELATED"))
    if (dosing) {
      # These variables are not output by default in mrgsolve when dosing is TRUE
      outvars_ <- unique(c(outvars_, "EVID", "CMT", "AMT"))
    }
    engineModel <- structuralModel %>% export(dest="mrgsolve", outvars=outvars_, extra_params=c(etaNames, declare))
    
    # Disable IIV in mrgsolve model
    engineModel@omega <- character(0) # IIV managed by CAMPSIS
  }
  
  # Compute all slice rounds to perform
  sliceRounds <- list(start=seq(1, maxID, by=slices), end=seq(0, maxID-1, by=slices) + slices)
  
  # Prepare all subdatasets (1 event dataframe per slice/round)
  subdatasets <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
    subdataset <- dataset %>% dplyr::filter(.data$ID >= .x & .data$ID <= .y)
    return(subdataset)
  })
  
  # Update number of slices in progress
  progress <- processExtraArg(args, name="progress", default=NULL, mandatory=TRUE)
  progress@slices <- subdatasets %>% length()
  
  # Compartment names
  cmtNames <- model@compartments@list %>% purrr::map_chr(~.x %>% toString())
  
  return(list(declare=declare, engineModel=engineModel, subdatasets=subdatasets,
              dropOthers=dropOthers, iteration=iteration, cmtNames=cmtNames, progress=progress))
}

#' Get initial conditions at simulation start-up.
#' 
#' @param subdataset subset of the dataset to simulate
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
    inits <- iteration@inits %>% dplyr::filter(.data$ID==currentID) %>% unlist()
    inits <- inits[cmtNames]
  }
  return(inits)
}

#' Reorder output columns.
#' 
#' @param results RxODE/mrgsolve output
#' @param dosing dosing information, logical value
#' @return reordered dataframe
#' @importFrom dplyr relocate any_of
#' @keywords internal
#' 
reorderColumns <- function(results, dosing) {
  # Use of any_of with relocate because ARM column may not be there if simulate
  # is used with a 2-dimensional dataset
  if (dosing) {
    results <- results %>% dplyr::relocate(dplyr::any_of(c("ID", "EVID", "CMT", "AMT", "TIME", "SCENARIO", "ARM")))
  } else {
    results <- results %>% dplyr::relocate(dplyr::any_of(c("ID", "TIME", "SCENARIO", "ARM")))
  }
  return(results)
}

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "rxode_engine", "events", "scenarios", "function", "character", "function", "integer", "integer", "logical", "logical"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, nocb, dosing, ...) {
  
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  summary <- processExtraArg(list(...), name="summary", default=DatasetSummary(), mandatory=TRUE)
  
  # Retrieve simulation config
  config <- processSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, ...)

  # Instantiate RxODE model
  rxmod <- config$engineModel
  if (dest@rxode2) {
    mod <- rxode2::rxode2(paste0(rxmod@code, collapse="\n"))
  } else {
    # For backwards compatibility since RxODE isn't on CRAN anymore
    fun <- getExportedValue("RxODE", "RxODE")
    mod <- do.call(fun, list(paste0(rxmod@code, collapse="\n")))
  }
  
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

  results <- purrr::map2_df(.x=config$subdatasets, .y=seq_along(config$subdatasets), .f=function(subdataset, index) {
    inits <- getInitialConditions(subdataset, iteration=config$iteration, cmtNames=config$cmtNames)

    # Launch simulation with RxODE
    if (dest@rxode2) {
      tmp <- rxode2::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=subdataset, returnType="tibble",
                             keep=keep, inits=inits, covsInterpolation=ifelse(nocb, "nocb", "locf"), addDosing=dosing, addCov=FALSE)
    } else {
      # For backwards compatibility since RxODE isn't on CRAN anymore
      fun <- getExportedValue("RxODE", "rxSolve")
      tmp <- do.call(fun, list(object=mod, params=params, omega=omega, sigma=sigma, events=subdataset, returnType="tibble",
                               keep=keep, inits=inits, covs_interpolation=ifelse(nocb, "nocb", "constant"), addDosing=dosing))
    }
    
    # Tick progress
    config$progress <- config$progress %>% updateSlice(index)
    config$progress <- config$progress %>% tick()
    
    # RxODE does not add the 'ID' column if only 1 subject
    if (!("id" %in% colnames(tmp))) {
      tmp <- tmp %>% tibble::add_column(ID=unique(subdataset$ID), .before=1) %>% dplyr::rename(TIME="time")
    } else {
      # Use same ID and TIME columns as NONMEM/mrgsolve
      tmp <- tmp %>% dplyr::rename(ID="id", TIME="time")
    }
    if (dosing) {
      # Rename dosing-related columns
      tmp <- tmp %>% dplyr::rename(EVID="evid", CMT="cmt", AMT="amt")
    }
    
    return(processDropOthers(tmp, outvars=outvars, dropOthers=config$dropOthers))
  })
  return(results %>% reorderColumns(dosing=dosing))
})

#' @importFrom purrr map2_df
#' @importFrom digest sha1
#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "mrgsolve_engine", "events", "scenarios", "function", "character", "function", "integer", "integer", "logical", "logical"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, nocb, dosing, ...) {
  
  # Retrieve simulation config
  config <- processSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, dosing=dosing, ...)
  
  # Retrieve mrgsolve model
  mrgmod <- config$engineModel
  
  mrgmodCode <- mrgmod %>% toString()
  mrgmodHash <- digest::sha1(mrgmodCode)
  
  # Instantiate mrgsolve model
  withCallingHandlers({
    mod <- mrgsolve::mcode_cache(model=paste0("mod_", mrgmodHash), code=mrgmodCode, quiet=TRUE)
  }, message = function(msg) {
    if (msg$message %>% startsWith("(waiting)"))
      invokeRestart("muffleMessage")
  })
  
  # Retrieve THETA's
  thetas <- model@parameters %>% select("theta")
  thetaParams <- thetas@list %>%
    purrr::set_names(thetas@list %>% purrr::map_chr(~.x %>% getNameInModel)) %>%
    purrr::map(~.x@value)

  results <-  purrr::map2_df(.x=config$subdatasets, .y=seq_along(config$subdatasets), .f=function(subdataset, index) {
    inits <- getInitialConditions(subdataset, iteration=config$iteration, cmtNames=config$cmtNames)

    # Update init vector (see mrgsolve script: 'update.R')
    if (!is.null(inits)) {
      mod <- mod %>% mrgsolve::update(init=inits)
    }
    
    # Inject THETA's into model
    if (length(thetaParams) > 0) {
      mod <- mod %>% mrgsolve::update(param=thetaParams)
    }
    
    # Launch simulation with mrgsolve
    # Observation only set to TRUE to align results with RxODE
    tmp <- mod %>% mrgsolve::data_set(data=subdataset) %>% mrgsolve::mrgsim(obsonly=!dosing, output="df", nocb=nocb) %>% tibble::as_tibble()

    # Tick progress
    config$progress <- config$progress %>% updateSlice(index)
    config$progress <- config$progress %>% tick()
    
    return(processDropOthers(tmp, outvars=outvars, dropOthers=config$dropOthers))
  })
  return(results %>% reorderColumns(dosing=dosing))
})
