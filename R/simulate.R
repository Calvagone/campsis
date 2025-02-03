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
#' @param outfun an output function to apply on the simulation results. Type ?Outfun for more info.
#' @param seed seed value
#' @param replicates number of replicates, default is 1
#' @param dosing output dosing information, default is FALSE
#' @param settings advanced simulation settings
#' @return dataframe with all results
#' @export
#' @rdname simulate
simulate <- function(model, dataset, dest=NULL, events=NULL, scenarios=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, dosing=FALSE, settings=NULL) {
  stop("No default function is provided")
}

setGeneric("simulate", function(model, dataset, dest=NULL, events=NULL, scenarios=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, dosing=FALSE, settings=NULL) {
  dest <- preprocessDest(dest)
  events <- preprocessEvents(events)
  scenarios <- preprocessScenarios(scenarios)
  tablefun <- preprocessTablefun(tablefun)
  outvars <- preprocessOutvars(outvars)
  outfun <- preprocessOutfun(outfun)
  seed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates, model)
  settings <- preprocessSettings(settings, dest)
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
exportTableDelegate <- function(model, dataset, dest, events, seed, tablefun, settings) {

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
    table <- dataset %>% export(dest=dest, model=model, seed=seed, settings=settings, event_related_column=TRUE)
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
#' @return a data frame with the results
#' @keywords internal
#' @importFrom dplyr across bind_rows group_by slice ungroup
#' 
simulateDelegateCore <- function(model, dataset, dest, events, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  destEngine <- getSimulationEngineType(dest)
  summary <- settings@internal@dataset_summary
  progress <- settings@internal@progress
  iterations <- settings@internal@iterations
  
  tableSeed <- getSeedForDatasetExport(seed=seed, progress=progress)
  table <- exportTableDelegate(model=model, dataset=dataset, dest=dest, events=events, seed=tableSeed, tablefun=tablefun, settings=settings)

  inits <- data.frame()
  results <- NULL
  for (iteration in iterations) {
    # Update iteration counter
    progress <- progress %>% updateIteration(iteration@index)
    
    iteration@inits <- inits
    table_ <- cutTableForEvent(table, iteration, summary)
    
    # Update internal settings
    settings@internal@progress <- progress
    settings@internal@iterations[[iteration@index]] <- iteration
    
    results_ <- simulate(model=model, dataset=table_, dest=destEngine, events=events, tablefun=tablefun,
                         outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, dosing=dosing,
                         settings=settings)
    # Shift times back to their original value
    results_$TIME <- results_$TIME + iteration@start
    
    # Store initial values for next iteration
    inits <- results_ %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::slice(which.max(.data$TIME))
    
    # Set seed for next simulation
    iterationSeed <- getSeedForIteration(seed=seed, progress=progress)
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
  return(results)
}

#' Process arm labels. Arm identifiers in ARM column are replaced by arm labels
#' as soon as one arm label is provided.
#' 
#' @param campsis CAMPSIS output
#' @param arms all treatment arms
#' @return updated CAMPSIS output with arm labels instead of arm identifiers
#' @importFrom dplyr mutate recode
#' @importFrom purrr map_chr map_int
#' @keywords internal
#' 
processArmLabels <- function(campsis, arms) {
  armIds <- arms@list %>% purrr::map_int(~.x@id)
  armLabels <- arms@list %>% purrr::map_chr(~.x@label)
  if (("ARM" %in% colnames(campsis)) && any(!is.na(armLabels))) {
    armLabels <- ifelse(is.na(armLabels), paste("ARM", armIds), armLabels)
    campsis <- campsis %>% dplyr::mutate(ARM=dplyr::recode(.data$ARM, !!!setNames(armLabels, armIds)))
  }
  return(campsis)
}

#' Simulation scenarios.
#' 
#' @inheritParams simulate
#' @return a data frame with the results
#' @keywords internal
#' @importFrom methods validObject
#' @importFrom furrr future_imap_dfr
simulateScenarios <- function(scenarios, model, dataset, dest, events,
                              tablefun, outvars, outfun, seed, replicates,
                              dosing, settings) {
  emptyScenarios <- scenarios %>% length() == 0
  if (emptyScenarios) {
    scenarios <- scenarios %>%
      add(Scenario())
  }
  
  outer <-  furrr::future_imap_dfr(.x=scenarios@list, .f=function(scenario, scenarioIndex) {
    model <- model %>% applyScenario(scenario)
    dataset <- dataset %>% applyScenario(scenario)
    
    # Validate CAMPSIS model in depth
    methods::validObject(model, complete=TRUE)
    
    # Validate CAMPSIS dataset in depth (btw, validObject also works on non S4 objects)
    methods::validObject(dataset, complete=TRUE)
    
    # Find out how many iterations are needed
    maxTime <- getDatasetMaxTime(dataset)
    iterations <- getEventIterations(events, maxTime=maxTime)
    settings@internal@iterations <- iterations
    
    # Update number of iterations in progress object
    settings@internal@progress@iterations <- iterations %>% length()
    
    # Update scenario counter
    settings@internal@progress <- settings@internal@progress %>% updateScenario(scenarioIndex)
    
    # Make short summary of dataset
    if (is(dataset, "dataset")) {
      settings@internal@dataset_summary <- toDatasetSummary(dataset)
    }
    
    inner <- simulateDelegateCore(model=model, dataset=dataset, dest=dest, events=events,
                                    tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                                    dosing=dosing, settings=settings)
    
    # Apply potential output function
    inner <- inner %>% applyOutfun(outfun=outfun, level="scenario", scenario=scenario@name)
    
    # Add column SCENARIO if scenarios were provided (at least 1)
    if (!emptyScenarios) {
      inner <- inner %>% dplyr::mutate(SCENARIO=scenario@name)
    }

    return(inner)
  }, .options=furrr::furrr_options(seed=NULL, scheduling=getFurrrScheduling(settings@hardware@scenario_parallel)))
  
  # Label arms (ARM column)
  if (is(dataset, "dataset")) {
    outer <- processArmLabels(outer, dataset@arms)
  }

  return(outer)
}

#' Simulation delegate (several replicates).
#' 
#' @inheritParams simulate
#' @return a data frame with the results
#' @keywords internal
#' @importFrom furrr furrr_options future_imap_dfr
#' @importFrom progressr progressor
#' @importFrom dplyr all_of mutate
#' 
simulateDelegate <- function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {

  # Setup plan automatically if parallel computing is required
  if (settings@hardware@auto_setup_plan) {
    setupPlanDefault(settings@hardware)
  }
  
  # Create progressor
  p <- progressr::progressor(steps=100)
  
  # Record progress
  scenariosLength <- scenarios %>% length()
  settings@internal@progress <- SimulationProgress(replicates=replicates, scenarios=ifelse(scenariosLength > 0, scenariosLength, 1),
                                                   progressor=p, hardware=settings@hardware)
  
  # Check model type
  if (is(model, "replicated_campsis_model")) {
    replicatedModel <- model
  } else if (is(model, "campsis_model")) {
    setSeed(getSeedForParametersSampling(seed=seed))
    if (replicates > 1) {
      replicatedModel <- model %>% replicate(n=replicates)
    } else {
      replicatedModel <- new("replicated_campsis_model", original_model=model)
    }
  } else {
    stop("Model must be of type 'campsis_model' or 'replicated_campsis_model'")
  }

  # Run all models
  seqReplicates <- seq_len(replicates)
  seqReplicates <- as.list(seqReplicates) %>%
    setNames(seqReplicates) # Names are added for furrr (added automatically to the output with .id="replicate")
  
  allRep <- seqReplicates %>% furrr::future_map_dfr(.f=function(replicate) {
    # Export model for each replicate
    model_ <- replicatedModel %>%
      campsismod::export(dest=CampsisModel(), index=replicate)
    
    # Update replicate counter
    settings@internal@progress <- settings@internal@progress %>% updateReplicate(replicate)
    retValue <- tryCatch(
      expr={
        inner <- simulateScenarios(scenarios=scenarios, model=model_, dataset=dataset, dest=dest, events=events,
                                     tablefun=tablefun, outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                                     dosing=dosing, settings=settings)
        # Apply potential output function
        inner <- inner %>% applyOutfun(outfun=outfun, level="replicate", replicate=replicate) 
      },
      error=function(cond) {
        if (replicates==1) {
          stop(cond)
        } else {
          message(paste0("Error with replicate number ", replicate))
          if (replicate==1) message(cond$message) # Only print error message for the first replicate
        }
        return(NULL)
      }
    )
    return(retValue)
  }, .id="replicate", .options=furrr::furrr_options(seed=NULL, scheduling=getFurrrScheduling(settings@hardware@replicate_parallel)))
  
  # Remove 'replicate' column if only 1 replicate
  if (replicates==1) {
    allRep <- allRep %>% dplyr::select(-dplyr::all_of("replicate"))
  } else {
    allRep <- allRep %>% dplyr::mutate(replicate=as.integer(.data$replicate))
  }
  
  return(allRep)
}

#' @rdname simulate
setMethod("simulate", signature=c("replicated_campsis_model", "dataset", "character", "events", "scenarios", "function", "character", "output_function", "integer", "integer", "logical", "simulation_settings"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, scenarios=scenarios, tablefun=tablefun,
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, dosing=dosing, settings=settings))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "dataset", "character", "events", "scenarios", "function", "character", "output_function", "integer", "integer", "logical", "simulation_settings"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, scenarios=scenarios, tablefun=tablefun,
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, dosing=dosing, settings=settings))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "character", "events", "scenarios", "function", "character", "output_function", "integer", "integer", "logical", "simulation_settings"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, events=events, scenarios=scenarios, tablefun=tablefun, 
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, dosing=dosing, settings=settings))
})

#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "data.frame", "character", "events", "scenarios", "function", "character", "output_function", "integer", "integer", "logical", "simulation_settings"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  return(simulateDelegate(model=model, dataset=tibble::as_tibble(dataset), dest=dest, events=events, scenarios=scenarios, tablefun=tablefun, 
                                    outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, dosing=dosing, settings=settings))
})

#' Remove initial conditions.
#' 
#' @param model CAMPSIS model
#' @return same model without initial conditions
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
#' @param settings simulation settings
#' @return a simulation configuration
#' @importFrom purrr map2
#' @keywords internal
#' 
processSimulateArguments <- function(model, dataset, dest, outvars, dosing, settings) {

  # Retrieve current iteration
  iteration <- settings@internal@iterations[[settings@internal@progress@iteration]]

  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Events do no support multiple individuals per slice -> always 1
  # Otherwise, the number of subjects per slice is defined in the hardware settings
  if (iteration@maxIndex > 1) {
    slices <- 1
  } else {
    slices <- preprocessSlices(settings@hardware@slice_size, maxID=maxID)
  }
  
  # Drop others 'argument'
  dropOthers <- dropOthers() %in% outvars

  # Extra argument declare (for mrgsolve only)
  user_declare <- settings@declare@variables
  summary <- settings@internal@dataset_summary
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
    
    # Export structural model (all THETAs to 0, all OMEGAs to 0, all SIGMAs to 0)
    structuralModel <- model
    structuralModel@parameters@list <- structuralModel@parameters@list %>% purrr::map(.f=function(parameter) {
      parameter@value <- 0
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

  # Compartment names
  cmtNames <- model@compartments@list %>% purrr::map_chr(~.x %>% toString())
  
  return(list(declare=declare, engineModel=engineModel, subdatasets=subdatasets,
              dropOthers=dropOthers, iteration=iteration, cmtNames=cmtNames))
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

#' @importFrom furrr future_imap_dfr furrr_options
#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "rxode_engine", "events", "scenarios", "function", "character", "output_function", "integer", "integer", "logical", "simulation_settings"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  summary <- settings@internal@dataset_summary
  
  # Retrieve simulation config
  config <- processSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, settings=settings)
  progress <- settings@internal@progress
  progress@slices <- config$subdatasets %>% length()

  # Instantiate RxODE model
  rxmod <- config$engineModel
  if (dest@rxode2) {
    mod <- rxode2::rxode2(model=paste0(rxmod@code, collapse="\n"), envir=NULL)
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
  if (dest@rxode2) {
    omega <- FALSE
  } else {
    omega <- matrix(1)
    fakeEtaName <- "FAKE_ETA"
    rownames(omega) <- fakeEtaName
    colnames(omega) <- fakeEtaName
  }
  
  # Prepare simulation
  keep <- outvars[outvars %in% c(summary@covariate_names, summary@iov_names, colnames(rxmod@omega))]
  solver <- settings@solver # Solver settings
  nocb <- settings@nocb@enable
  tick_slice <- settings@progress@tick_slice
  
  # Make sure to remove the list of sub-datasets from 'config' (see #166)
  subdatasets <- config$subdatasets
  config$subdatasets <- NULL

  results <- furrr::future_imap_dfr(.x=subdatasets, .f=function(subdataset, index) {
    inits <- getInitialConditions(subdataset, iteration=config$iteration, cmtNames=config$cmtNames)

    # Launch simulation with RxODE
    if (dest@rxode2) {
      tmp <- rxode2::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=subdataset, returnType="tibble",
                             atol=solver@atol, rtol=solver@rtol, hmax=solver@hmax, maxsteps=solver@maxsteps, method=solver@method,
                             keep=keep, inits=inits, covsInterpolation=ifelse(nocb, "nocb", "locf"), addDosing=dosing, addCov=FALSE, cores=1)
    } else {
      # For backwards compatibility since RxODE isn't on CRAN anymore
      fun <- getExportedValue("RxODE", "rxSolve")
      tmp <- do.call(fun, list(object=mod, params=params, omega=omega, sigma=sigma, events=subdataset, returnType="tibble",
                               atol=solver@atol, rtol=solver@rtol, hmax=solver@hmax, maxsteps=solver@maxsteps, method=solver@method,
                               keep=keep, inits=inits, covs_interpolation=ifelse(nocb, "nocb", "constant"), addDosing=dosing, cores=1))
    }
    
    # Tick progress
    if (tick_slice) {
      progress <- progress %>% updateSlice(index)
      progress <- progress %>% tick(tick_slice=tick_slice)
    }
    
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
  }, .options=furrr::furrr_options(seed=TRUE, scheduling=getFurrrScheduling(settings@hardware@slice_parallel)))
  
  # Tick progress
  if (!tick_slice) {
    progress <- progress %>% updateSlice(subdatasets %>% length())
    progress <- progress %>% tick(tick_slice=tick_slice)
  }
  
  return(results %>% reorderColumns(dosing=dosing))
})

#' @importFrom furrr future_imap_dfr furrr_options
#' @importFrom digest sha1
#' @rdname simulate
setMethod("simulate", signature=c("campsis_model", "tbl_df", "mrgsolve_engine", "events", "scenarios", "function", "character", "output_function", "integer", "integer", "logical", "simulation_settings"),
          definition=function(model, dataset, dest, events, scenarios, tablefun, outvars, outfun, seed, replicates, dosing, settings) {
  
  # Retrieve simulation config
  config <- processSimulateArguments(model=model, dataset=dataset, dest=dest, outvars=outvars, dosing=dosing, settings=settings)
  progress <- settings@internal@progress
  progress@slices <- config$subdatasets %>% length()
  
  # Retrieve mrgsolve model
  mrgmod <- config$engineModel
  
  mrgmodCode <- mrgmod %>% toString()
  mrgmodHash <- digest::sha1(mrgmodCode)
  
  # Instantiate mrgsolve model
  mod <- mrgsolve::mcode_cache(model=paste0("mod_", mrgmodHash), code=mrgmodCode, quiet=TRUE)
  
  # Retrieve THETA's
  thetas <- model@parameters %>% select("theta")
  thetaParams <- thetas@list %>%
    purrr::set_names(thetas@list %>% purrr::map_chr(~.x %>% getNameInModel)) %>%
    purrr::map(~.x@value)
  
  # Apply simulation settings
  solver <- settings@solver
  mod <- mod %>% mrgsolve::update(atol=solver@atol, rtol=solver@rtol, hmax=solver@hmax, maxsteps=solver@maxsteps)
  nocb <- settings@nocb@enable
  tick_slice <- settings@progress@tick_slice

  # Make sure to remove the list of sub-datasets from 'config' (see #166)
  subdatasets <- config$subdatasets
  config$subdatasets <- NULL
  
  # Inject THETA's into model
  if (length(thetaParams) > 0) {
    mod <- mod %>% mrgsolve::update(param=thetaParams)
  }
  
  # Inject SIGMA's into model (RUV managed by mrgsolve)
  sigma <- campsismod::rxodeMatrix(model=model, type="sigma")
  if (nrow(sigma) > 0) {
    mod <- mod %>% mrgsolve::update(sigma=sigma)
  }

  results <-  furrr::future_imap_dfr(.x=subdatasets, .f=function(subdataset, index) {
    inits <- getInitialConditions(subdataset, iteration=config$iteration, cmtNames=config$cmtNames)

    # Update init vector (see mrgsolve script: 'update.R')
    if (!is.null(inits)) {
      mod <- mod %>% mrgsolve::update(init=inits)
    }
    
    # Launch simulation with mrgsolve
    # Observation only set to TRUE to align results with RxODE
    tmp <- mod %>% mrgsolve::data_set(data=subdataset) %>% mrgsolve::mrgsim(obsonly=!dosing, output="df", nocb=nocb) %>% tibble::as_tibble()

    # Tick progress
    if (tick_slice) {
      progress <- progress %>% updateSlice(index)
      progress <- progress %>% tick(tick_slice=tick_slice)
    }

    return(processDropOthers(tmp, outvars=outvars, dropOthers=config$dropOthers))
  }, .options=furrr::furrr_options(seed=TRUE, scheduling=getFurrrScheduling(settings@hardware@slice_parallel)))
  
  # Tick progress
  if (!tick_slice) {
    progress <- progress %>% updateSlice(subdatasets %>% length())
    progress <- progress %>% tick(tick_slice=tick_slice)
  }
  
  return(results %>% reorderColumns(dosing=dosing))
})
