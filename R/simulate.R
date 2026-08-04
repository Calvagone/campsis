#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

#' Simulate function.
#'
#' @param model generic Campsis model
#' @param dataset Campsis dataset or 2-dimensional table
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
simulate <- function(
  model,
  dataset,
  dest = NULL,
  events = NULL,
  scenarios = NULL,
  tablefun = NULL,
  outvars = NULL,
  outfun = NULL,
  seed = NULL,
  replicates = NULL,
  dosing = FALSE,
  settings = NULL
) {
  no_default_function_provided_debug(mget(names(formals()), envir = environment()), "simulate")
}

setGeneric(
  "simulate",
  function(
    model,
    dataset,
    dest = NULL,
    events = NULL,
    scenarios = NULL,
    tablefun = NULL,
    outvars = NULL,
    outfun = NULL,
    seed = NULL,
    replicates = NULL,
    dosing = FALSE,
    settings = NULL
  ) {
    if (is.null(settings)) {
      settings <- Settings()
    }

    # Propagate default settings
    default_settings <- settings@default
    if (is.null(dest)) {
      dest <- default_settings@engine
    }
    if (is.null(seed)) {
      seed <- default_settings@seed
    }
    if (is.null(replicates)) {
      replicates <- default_settings@replicates
    }
    if (is.null(outvars)) {
      outvars <- default_settings@outvars
    }
    if (is.null(dosing)) {
      dosing <- default_settings@dosing
    }
    if (is.null(outfun)) {
      outfun <- default_settings@outfuns
    }

    dest <- preprocess_dest(dest)
    events <- preprocess_events(events)
    scenarios <- preprocess_scenarios(scenarios)
    tablefun <- preprocess_tablefun(tablefun)
    outvars <- preprocess_outvars(outvars)
    outfun <- preprocess_outfun(outfun)
    seed <- get_seed(seed)
    replicates <- preprocess_replicates(replicates, model)
    dosing <- preprocess_dosing(dosing)
    settings <- preprocess_settings(settings, dest)

    return(standardGeneric("simulate"))
  }
)

#' Get simulation engine type.
#'
#' @param dest destination engine, string form
#' @return simulation engine type
#' @keywords internal
#'
get_simulation_engine_type <- function(dest) {
  if (dest == "rxode2") {
    engine <- new("rxode_engine")
  } else if (dest == "mrgsolve") {
    engine <- new("mrgsolve_engine")
  } else {
    stop("Only rxode2 and mrgsolve are supported for now")
  }
  return(engine)
}

#' Export table delegate.
#'
#' @inheritParams simulate
#' @return a data frame
#' @keywords internal
#'
export_table_delegate <- function(model, dataset, dest, events, seed, tablefun, settings) {
  if (is(dataset, "dataset")) {
    # Retrieve event times (same for all arms)
    eventTimes <- c(0, events %>% get_times()) %>% unique()

    # Add all 'event-related' times in each arm
    for (armIndex in seq_len(dataset@arms %>% length())) {
      arm <- dataset@arms@list[[armIndex]]
      obsTimes <- arm %>% get_times()
      if (obsTimes %>% length() == 0) {
        stop(paste0("Arm ", arm@id, " does not contain any observation."))
      }
      eventRelatedTimes <- eventTimes[!(eventTimes %in% obsTimes)]
      if (eventRelatedTimes %>% length() > 0) {
        eventRelatedObs <- EventRelatedObservations(times = eventRelatedTimes, compartment = NA)
        dataset@arms@list[[armIndex]] <- dataset@arms@list[[armIndex]] %>% add(eventRelatedObs)
      }
    }
    table <- dataset %>%
      export(dest = dest, model = model, seed = seed, settings = settings, event_related_column = TRUE)
  } else {
    table <- dataset
    if (!("EVENT_RELATED" %in% colnames(table))) {
      table <- table %>% dplyr::mutate(EVENT_RELATED = as.integer(FALSE))
    }
  }
  table <- tablefun(table)
  return(table)
}

#' Simulation delegate core (single replicate).
#'
#' @inheritParams simulate
#' @return a data frame with the results
#' @keywords internal
#' @importFrom dplyr across bind_rows group_by slice ungroup
#'
simulate_delegate_core <- function(
  model,
  dataset,
  dest,
  events,
  tablefun,
  outvars,
  outfun,
  seed,
  replicates,
  dosing,
  settings
) {
  destEngine <- get_simulation_engine_type(dest)
  summary <- settings@internal@dataset_summary
  progress <- settings@internal@progress
  iterations <- settings@internal@iterations

  tableSeed <- get_seed_for_dataset_export(seed = seed, progress = progress)
  table <- export_table_delegate(
    model = model,
    dataset = dataset,
    dest = dest,
    events = events,
    seed = tableSeed,
    tablefun = tablefun,
    settings = settings
  )

  inits <- data.frame()
  results <- NULL
  for (iteration in iterations) {
    # Update iteration counter
    progress <- progress %>% update_iteration(iteration@index)

    iteration@inits <- inits
    table_ <- cutTableForEvent(table, iteration, summary)

    # Update internal settings
    settings@internal@progress <- progress
    settings@internal@iterations[[iteration@index]] <- iteration

    results_ <- simulate(
      model = model,
      dataset = table_,
      dest = destEngine,
      events = events,
      tablefun = tablefun,
      outvars = outvars,
      outfun = outfun,
      seed = seed,
      replicates = replicates,
      dosing = dosing,
      settings = settings
    )
    # Shift times back to their original value
    results_$TIME <- results_$TIME + iteration@start

    # Store initial values for next iteration
    inits <- results_ %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::slice(which.max(.data$TIME))

    # Set seed for next simulation
    iterationSeed <- get_seed_for_iteration(seed = seed, progress = progress)
    set_seed(iterationSeed)

    # Calling events
    for (event in events@list) {
      if (iteration@end %in% as.numeric(event@times)) {
        inits <- event@fun(inits)
      }
    }

    # Get rid of event related observations and remove column
    results_ <- results_ %>% dplyr::filter(.data$EVENT_RELATED == 0) %>% dplyr::select(-dplyr::all_of("EVENT_RELATED"))

    # Append simulation results to global results
    # Except for iteration 1 from 0 to 0 which is a special case
    if (!(iteration@index == 1 && iteration@start == 0 && iteration@end == 0 && iteration@maxIndex > 1)) {
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
#' @param campsis Campsis output
#' @param arms all treatment arms
#' @return updated Campsis output with arm labels instead of arm identifiers
#' @importFrom dplyr mutate recode
#' @importFrom purrr map_chr map_int
#' @keywords internal
#'
process_arm_labels <- function(campsis, arms) {
  armIds <- arms@list %>% purrr::map_int(~ .x@id)
  armLabels <- arms@list %>% purrr::map_chr(~ .x@label)
  if (("ARM" %in% colnames(campsis)) && any(!is.na(armLabels))) {
    armLabels <- ifelse(is.na(armLabels), paste("ARM", armIds), armLabels)
    campsis <- campsis %>% dplyr::mutate(ARM = dplyr::recode(.data$ARM, !!!setNames(armLabels, armIds)))
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
#' @importFrom purrr imap_dfr
simulate_scenarios <- function(
  scenarios,
  model,
  dataset,
  dest,
  events,
  tablefun,
  outvars,
  outfun,
  seed,
  replicates,
  dosing,
  settings
) {
  emptyScenarios <- scenarios %>% length() == 0
  if (emptyScenarios) {
    scenarios <- scenarios %>%
      add(Scenario())
  }

  scenarioFun <- function(scenario, scenarioIndex) {
    model <- model %>% apply_scenario(scenario)
    dataset <- dataset %>% apply_scenario(scenario)

    # Validate Campsis model in depth
    methods::validObject(model, complete = TRUE)

    # Validate Campsis dataset in depth (btw, validObject also works on non S4 objects)
    methods::validObject(dataset, complete = TRUE)

    # Find out how many iterations are needed
    iterations <- getEventIterations(events, dataset = dataset)
    settings@internal@iterations <- iterations

    # Update number of iterations in progress object
    settings@internal@progress@iterations <- iterations %>% length()

    # Update scenario counter
    settings@internal@progress <- settings@internal@progress %>% update_scenario(scenarioIndex)

    # Make short summary of dataset
    if (is(dataset, "dataset")) {
      settings@internal@dataset_summary <- toDatasetSummary(dataset)
    }

    inner <- simulate_delegate_core(
      model = model,
      dataset = dataset,
      dest = dest,
      events = events,
      tablefun = tablefun,
      outvars = outvars,
      outfun = outfun,
      seed = seed,
      replicates = replicates,
      dosing = dosing,
      settings = settings
    )

    # Add column SCENARIO if scenarios were provided (at least 1)
    if (!emptyScenarios) {
      inner <- inner %>%
        dplyr::mutate(SCENARIO = scenario@name)
    }

    return(inner)
  }

  # Use 'future' only when required
  mapFun <- if (settings@hardware@scenario_parallel && settings@hardware@cpu > 1) {
    function(.x) {
      furrr::future_imap_dfr(.x = .x, .f = scenarioFun, .options = furrr::furrr_options(seed = NULL))
    }
  } else {
    function(.x) {
      purrr::imap_dfr(.x = .x, .f = scenarioFun)
    }
  }

  outer <- scenarios@list %>% mapFun()

  # Label arms (ARM column)
  if (is(dataset, "dataset")) {
    outer <- process_arm_labels(outer, dataset@arms)
  }

  return(outer)
}

#' Simulation delegate (several replicates).
#'
#' @inheritParams simulate
#' @return a data frame with the results
#' @keywords internal
#' @importFrom furrr furrr_options future_imap_dfr
#' @importFrom purrr map map_chr set_names imap_dfr
#' @importFrom progressr progressor
#' @importFrom dplyr all_of mutate
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#'
simulate_delegate <- function(
  model,
  dataset,
  dest,
  events,
  scenarios,
  tablefun,
  outvars,
  outfun,
  seed,
  replicates,
  dosing,
  settings
) {
  # Setup plan automatically if parallel computing is required
  if (settings@hardware@auto_setup_plan) {
    setup_plan_default(settings@hardware)
  }

  # Create progressor
  p <- progressr::progressor(steps = 100)

  # Record progress
  scenariosLength <- scenarios %>% length()
  settings@internal@progress <- SimulationProgress(
    replicates = replicates,
    scenarios = ifelse(scenariosLength > 0, scenariosLength, 1),
    progressor = p,
    hardware = settings@hardware
  )

  # Check model type
  if (is(model, "replicated_campsis_model")) {
    replicatedModel <- model
  } else if (is(model, "campsis_model")) {
    set_seed(get_seed_for_parameters_sampling(seed = seed))
    if (replicates > 1) {
      replicatedModel <- model %>%
        replicate(n = replicates, settings = settings@replication)
    } else {
      replicatedModel <- new("replicated_campsis_model", original_model = model)
    }
  } else {
    stop("Model must be of type 'campsis_model' or 'replicated_campsis_model'")
  }

  # Run all models
  seqReplicates <- seq_len(replicates)
  seqReplicates <- as.list(seqReplicates) %>%
    stats::setNames(seqReplicates) # Names are added for furrr (added automatically to the output with .id="replicate")

  repFun <- function(replicate) {
    # Export model for each replicate
    model_ <- replicatedModel %>%
      campsismod::export(dest = CampsisModel(), index = replicate)

    # Disable variabilities ('IIV', 'RUV', etc.)
    disabledVariabilities <- settings@default@disabled_variabilities
    if (length(disabledVariabilities) > 0) {
      model_ <- model_ %>%
        disable(disabledVariabilities)
    }

    # Update replicate counter
    settings@internal@progress <- settings@internal@progress %>% update_replicate(replicate)
    retValue <- tryCatch(
      expr = {
        inner <- simulate_scenarios(
          scenarios = scenarios,
          model = model_,
          dataset = dataset,
          dest = dest,
          events = events,
          tablefun = tablefun,
          outvars = outvars,
          outfun = outfun,
          seed = seed,
          replicates = replicates,
          dosing = dosing,
          settings = settings
        )
        # Apply potential output functions
        inner_list <- apply_outfun(x = inner, outfun = outfun, level = "replicate", replicate = replicate)

        # Wrap each result as a list-column so tidyr::unnest() downstream works
        # regardless of whether different output functions produce different row counts
        inner_list %>% purrr::map(~ list(.x)) %>% tibble::as_tibble()
      },
      error = function(cond) {
        if (replicates == 1) {
          stop(cond)
        } else {
          message(paste0("Error with replicate number ", replicate))
          if (replicate == 1) message(cond$message) # Only print error message for the first replicate
        }
        return(NULL)
      }
    )
    return(retValue)
  }

  mapFun <- if (settings@hardware@replicate_parallel && settings@hardware@cpu > 1) {
    function(.x) {
      furrr::future_map_dfr(.x = .x, .f = repFun, .id = "replicate", .options = furrr::furrr_options(seed = NULL))
    }
  } else {
    function(.x) {
      purrr::map_dfr(.x = .x, .f = repFun, .id = "replicate")
    }
  }

  all_rep_nested <- seqReplicates %>% mapFun()

  # Nested dataframe to list
  outfun_names <- outfun %>% get_names()
  all_rep_list <- outfun_names %>%
    purrr::set_names() %>%
    purrr::map(function(name) {
      tmp <- all_rep_nested %>%
        dplyr::select(replicate, !!rlang::sym(name)) %>%
        tidyr::unnest(!!rlang::sym(name)) %>%
        dplyr::mutate(replicate = as.integer(.data$replicate))
      if (replicates == 1) {
        tmp <- tmp %>%
          dplyr::select(-dplyr::all_of("replicate"))
      }
      metadata <- new(
        "campsis_metadata",
        dataset = if (is(dataset, "dataset")) {
          dataset
        } else {
          Dataset()
        },
        dest = dest,
        scenarios = scenarios,
        outvars = outvars,
        outfun = outfun %>% get_by_name(name),
        replicates = replicates
      )
      return(new_campsis_tbl(x = tmp, metadata = metadata))
    })

  if (outfun_names %>% length() == 1) {
    return(all_rep_list[[1]])
  }

  return(all_rep_list)
}

#' @rdname simulate
setMethod(
  "simulate",
  signature = c(
    "replicated_campsis_model",
    "dataset",
    "character",
    "events",
    "scenarios",
    "function",
    "character",
    "outfuns",
    "integer",
    "integer",
    "logical",
    "simulation_settings"
  ),
  definition = function(
    model,
    dataset,
    dest,
    events,
    scenarios,
    tablefun,
    outvars,
    outfun,
    seed,
    replicates,
    dosing,
    settings
  ) {
    return(simulate_delegate(
      model = model,
      dataset = dataset,
      dest = dest,
      events = events,
      scenarios = scenarios,
      tablefun = tablefun,
      outvars = outvars,
      outfun = outfun,
      seed = seed,
      replicates = replicates,
      dosing = dosing,
      settings = settings
    ))
  }
)

#' @rdname simulate
setMethod(
  "simulate",
  signature = c(
    "campsis_model",
    "dataset",
    "character",
    "events",
    "scenarios",
    "function",
    "character",
    "outfuns",
    "integer",
    "integer",
    "logical",
    "simulation_settings"
  ),
  definition = function(
    model,
    dataset,
    dest,
    events,
    scenarios,
    tablefun,
    outvars,
    outfun,
    seed,
    replicates,
    dosing,
    settings
  ) {
    return(simulate_delegate(
      model = model,
      dataset = dataset,
      dest = dest,
      events = events,
      scenarios = scenarios,
      tablefun = tablefun,
      outvars = outvars,
      outfun = outfun,
      seed = seed,
      replicates = replicates,
      dosing = dosing,
      settings = settings
    ))
  }
)

#' @rdname simulate
setMethod(
  "simulate",
  signature = c(
    "campsis_model",
    "tbl_df",
    "character",
    "events",
    "scenarios",
    "function",
    "character",
    "outfuns",
    "integer",
    "integer",
    "logical",
    "simulation_settings"
  ),
  definition = function(
    model,
    dataset,
    dest,
    events,
    scenarios,
    tablefun,
    outvars,
    outfun,
    seed,
    replicates,
    dosing,
    settings
  ) {
    return(simulate_delegate(
      model = model,
      dataset = dataset,
      dest = dest,
      events = events,
      scenarios = scenarios,
      tablefun = tablefun,
      outvars = outvars,
      outfun = outfun,
      seed = seed,
      replicates = replicates,
      dosing = dosing,
      settings = settings
    ))
  }
)

#' @rdname simulate
setMethod(
  "simulate",
  signature = c(
    "campsis_model",
    "data.frame",
    "character",
    "events",
    "scenarios",
    "function",
    "character",
    "outfuns",
    "integer",
    "integer",
    "logical",
    "simulation_settings"
  ),
  definition = function(
    model,
    dataset,
    dest,
    events,
    scenarios,
    tablefun,
    outvars,
    outfun,
    seed,
    replicates,
    dosing,
    settings
  ) {
    return(simulate_delegate(
      model = model,
      dataset = tibble::as_tibble(dataset),
      dest = dest,
      events = events,
      scenarios = scenarios,
      tablefun = tablefun,
      outvars = outvars,
      outfun = outfun,
      seed = seed,
      replicates = replicates,
      dosing = dosing,
      settings = settings
    ))
  }
)

#' Remove initial conditions.
#'
#' @param model Campsis model
#' @return same model without initial conditions
#' @keywords internal
#'
remove_initial_conditions <- function(model) {
  properties <- model@compartments@properties@list
  properties_ <- properties %>% purrr::keep(~ !is(.x, "compartment_initial_condition"))
  model@compartments@properties@list <- properties_
  return(model)
}

#' Preprocess arguments of the simulate method.
#'
#' @param model Campsis model
#' @param dataset dataset, data.frame form
#' @param dest destination engine
#' @param outvars outvars
#' @param dosing add dosing information, logical value
#' @param settings simulation settings
#' @return a simulation configuration
#' @importFrom purrr map2
#' @keywords internal
#'
process_simulate_arguments <- function(model, dataset, dest, outvars, dosing, settings) {
  # Retrieve current iteration
  iteration <- settings@internal@iterations[[settings@internal@progress@iteration]]

  # IDs
  ids <- preprocess_ids(dataset)
  maxID <- max(ids)

  # Events do no support multiple individuals per slice -> always 1
  # Otherwise, the number of subjects per slice is defined in the hardware settings
  if (iteration@maxIndex > 1) {
    slices <- 1
  } else {
    slices <- preprocess_slices(settings@hardware@slice_size, maxID = maxID)
  }

  # Drop others 'argument'
  drop_others <- drop_others() %in% outvars

  # Extra argument declare (for mrgsolve only)
  user_declare <- settings@declare@variables
  summary <- settings@internal@dataset_summary
  declare <- unique(c(
    summary@iov_names,
    summary@covariate_names,
    summary@occ_names,
    summary@tsld_tdos_names,
    user_declare,
    "ARM",
    "EVENT_RELATED"
  ))

  # Remove initial conditions from Campsis model before export (if present)
  if (iteration@index > 1) {
    model <- remove_initial_conditions(model)
  }

  # Compartment names
  cmtNames <- model@compartments@list %>% purrr::map_chr(~ .x %>% to_string())

  # Export to rxode2
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>%
      export(dest = "rxode2")

    # Export to mrgsolve
  } else if (is(dest, "mrgsolve_engine")) {
    # Export structural model (all THETAs to 0, all OMEGAs to 0, all SIGMAs to 0)
    structuralModel <- model
    structuralModel@parameters@list <- structuralModel@parameters@list %>%
      purrr::map(.f = function(parameter) {
        parameter@value <- 0
        return(parameter)
      })

    # Set ETA's as extra parameters in mrgsolve
    etaNames <- (model@parameters %>% select("omega"))@list %>%
      purrr::keep(~ is_diag(.x)) %>%
      purrr::map_chr(~ get_name_in_model(.x))

    # Extra care to additional outputs which need to be explicitly declared with mrgsolve
    outvars_ <- outvars[!(outvars %in% drop_others())]
    outvars_ <- outvars_[!outvars_ %in% cmtNames] # Exclude compartment names
    outvars_ <- unique(c(outvars_, "ARM", "EVENT_RELATED"))
    if (dosing) {
      # These variables are not output by default in mrgsolve when dosing is TRUE
      outvars_ <- unique(c(outvars_, "EVID", "CMT", "AMT"))
    }
    engineModel <- structuralModel %>%
      export(dest = "mrgsolve", outvars = outvars_, extra_params = c(etaNames, declare))

    # Disable IIV in mrgsolve model
    engineModel@omega <- character(0) # IIV managed by Campsis
  }

  # Compute all slice rounds to perform
  sliceRounds <- list(start = seq(1, maxID, by = slices), end = seq(0, maxID - 1, by = slices) + slices)

  # Prepare all subdatasets (1 event dataframe per slice/round)
  subdatasets <- purrr::map2(sliceRounds$start, sliceRounds$end, .f = function(.x, .y) {
    subdataset <- dataset %>% dplyr::filter(.data$ID >= .x & .data$ID <= .y)
    return(subdataset)
  })

  return(list(
    declare = declare,
    engineModel = engineModel,
    subdatasets = subdatasets,
    drop_others = drop_others,
    iteration = iteration,
    cmtNames = cmtNames
  ))
}

#' Get initial conditions at simulation start-up.
#'
#' @param subdataset subset of the dataset to simulate
#' @param iteration current iteration
#' @param cmtNames compartment names
#' @return named numeric vector with the new initial conditions
#' @keywords internal
#'
get_initial_conditions <- function(subdataset, iteration, cmtNames) {
  # Current ID is of length 1 or 6
  currentID <- unique(subdataset$ID) %>% as.integer()
  if (iteration@inits %>% nrow() == 0) {
    inits <- NULL
  } else {
    assertthat::assert_that(
      currentID %>% length() == 1,
      msg = paste0("Not a single ID: ", paste0(currentID, collapse = ","))
    )
    inits <- iteration@inits %>% dplyr::filter(.data$ID == currentID) %>% unlist()
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
reorder_columns <- function(results, dosing) {
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
#' @importFrom purrr imap_dfr
#' @rdname simulate
setMethod(
  "simulate",
  signature = c(
    "campsis_model",
    "tbl_df",
    "rxode_engine",
    "events",
    "scenarios",
    "function",
    "character",
    "outfuns",
    "integer",
    "integer",
    "logical",
    "simulation_settings"
  ),
  definition = function(
    model,
    dataset,
    dest,
    events,
    scenarios,
    tablefun,
    outvars,
    outfun,
    seed,
    replicates,
    dosing,
    settings
  ) {
    # Add ARM equation in model
    model <- preprocess_arm_column(dataset, model)
    summary <- settings@internal@dataset_summary

    # Retrieve simulation config
    config <- process_simulate_arguments(
      model = model,
      dataset = dataset,
      dest = dest,
      outvars = outvars,
      dosing = dosing,
      settings = settings
    )
    progress <- settings@internal@progress
    progress@slices <- config$subdatasets %>% length()

    # Instantiate RxODE model
    rxmod <- config$engineModel
    mod <- rxode2::rxode2(model = paste0(rxmod@code, collapse = "\n"), envir = NULL)

    # Preparing parameters
    params <- rxmod@theta
    sigma <- rxmod@sigma
    if (nrow(sigma) == 0) {
      sigma <- NULL
    }

    # Prepare simulation
    keep <- outvars[outvars %in% c(summary@covariate_names, summary@iov_names, colnames(rxmod@omega))]
    solver <- settings@solver # Solver settings
    nocb <- settings@nocb@enable
    tick_slice <- settings@progress@tick_slice

    # Make sure to remove the list of sub-datasets from 'config' (see #166)
    subdatasets <- config$subdatasets
    config$subdatasets <- NULL

    # This function will be called for each slice
    sliceFunRxode <- function(subdataset, index) {
      inits <- get_initial_conditions(subdataset, iteration = config$iteration, cmtNames = config$cmtNames)

      # Launch simulation with rxode2
      tmp <- rxode2::rxSolve(
          object = mod,
          params = params,
          omega = FALSE,
          sigma = sigma,
          events = subdataset,
          returnType = "tibble",
          atol = solver@atol,
          rtol = solver@rtol,
          hmax = solver@hmax,
          maxsteps = solver@maxsteps,
          method = solver@method,
          keep = keep,
          inits = inits,
          covsInterpolation = ifelse(nocb, "nocb", "locf"),
          addDosing = dosing,
          addCov = FALSE,
          cores = 1
        )

      # Tick progress
      if (tick_slice) {
        progress <- progress %>% update_slice(index)
        progress <- progress %>% tick(tick_slice = tick_slice)
      }

      # RxODE does not add the 'ID' column if only 1 subject
      if (!("id" %in% colnames(tmp))) {
        tmp <- tmp %>% tibble::add_column(ID = unique(subdataset$ID), .before = 1) %>% dplyr::rename(TIME = "time")
      } else {
        # Use same ID and TIME columns as NONMEM/mrgsolve
        tmp <- tmp %>% dplyr::rename(ID = "id", TIME = "time")
      }
      if (dosing) {
        # Rename dosing-related columns
        tmp <- tmp %>% dplyr::rename(EVID = "evid", CMT = "cmt", AMT = "amt")
      }

      return(process_drop_others(tmp, outvars = outvars, drop_others = config$drop_others))
    }

    # Use 'future' only when required
    mapFun <- if (settings@hardware@slice_parallel && settings@hardware@cpu > 1) {
      function(.x) {
        furrr::future_imap_dfr(.x = .x, .f = sliceFunRxode, .options = furrr::furrr_options(seed = TRUE))
      }
    } else {
      function(.x) {
        purrr::imap_dfr(.x = .x, .f = sliceFunRxode)
      }
    }

    results <- subdatasets %>% mapFun()

    # Tick progress
    if (!tick_slice) {
      progress <- progress %>% update_slice(subdatasets %>% length())
      progress <- progress %>% tick(tick_slice = tick_slice)
    }

    return(results %>% reorder_columns(dosing = dosing))
  }
)

#' @importFrom furrr future_imap_dfr furrr_options
#' @importFrom purrr imap_dfr
#' @importFrom digest sha1
#' @rdname simulate
setMethod(
  "simulate",
  signature = c(
    "campsis_model",
    "tbl_df",
    "mrgsolve_engine",
    "events",
    "scenarios",
    "function",
    "character",
    "outfuns",
    "integer",
    "integer",
    "logical",
    "simulation_settings"
  ),
  definition = function(
    model,
    dataset,
    dest,
    events,
    scenarios,
    tablefun,
    outvars,
    outfun,
    seed,
    replicates,
    dosing,
    settings
  ) {
    # Retrieve simulation config
    config <- process_simulate_arguments(
      model = model,
      dataset = dataset,
      dest = dest,
      outvars = outvars,
      dosing = dosing,
      settings = settings
    )
    progress <- settings@internal@progress
    progress@slices <- config$subdatasets %>% length()

    # Retrieve mrgsolve model
    mrgmod <- config$engineModel

    mrgmodCode <- mrgmod %>% to_string()
    mrgmodHash <- digest::sha1(mrgmodCode)

    # Instantiate mrgsolve model
    mod <- mrgsolve::mcode_cache(model = paste0("mod_", mrgmodHash), code = mrgmodCode, quiet = TRUE)

    # Retrieve THETA's
    thetas <- model@parameters %>% select("theta")
    thetaParams <- thetas@list %>%
      purrr::set_names(thetas@list %>% purrr::map_chr(~ .x %>% get_name_in_model)) %>%
      purrr::map(~ .x@value)

    # Apply simulation settings
    solver <- settings@solver
    mod <- mod %>%
      mrgsolve::update(atol = solver@atol, rtol = solver@rtol, hmax = solver@hmax, maxsteps = solver@maxsteps)
    nocb <- settings@nocb@enable
    tick_slice <- settings@progress@tick_slice

    # Make sure to remove the list of sub-datasets from 'config' (see #166)
    subdatasets <- config$subdatasets
    config$subdatasets <- NULL

    # Inject THETA's into model
    if (length(thetaParams) > 0) {
      mod <- mod %>% mrgsolve::update(param = thetaParams)
    }

    # Inject SIGMA's into model (RUV managed by mrgsolve)
    sigma <- campsismod::rxode_matrix(model = model, type = "sigma")
    if (nrow(sigma) > 0) {
      mod <- mod %>% mrgsolve::update(sigma = sigma)
    }

    # This function will be called for each slice
    sliceFunMrgsolve <- function(subdataset, index) {
      inits <- get_initial_conditions(subdataset, iteration = config$iteration, cmtNames = config$cmtNames)

      # Update init vector (see mrgsolve script: 'update.R')
      if (!is.null(inits)) {
        mod <- mod %>% mrgsolve::update(init = inits)
      }

      # Launch simulation with mrgsolve
      # Observation only set to TRUE to align results with RxODE
      tmp <- mod %>%
        mrgsolve::data_set(data = subdataset) %>%
        mrgsolve::mrgsim(obsonly = !dosing, output = "df", nocb = nocb) %>%
        tibble::as_tibble()

      # Tick progress
      if (tick_slice) {
        progress <- progress %>% update_slice(index)
        progress <- progress %>% tick(tick_slice = tick_slice)
      }

      return(process_drop_others(tmp, outvars = outvars, drop_others = config$drop_others))
    }

    # Use 'future' only when required
    mapFun <- if (settings@hardware@slice_parallel && settings@hardware@cpu > 1) {
      function(.x) {
        furrr::future_imap_dfr(.x = .x, .f = sliceFunMrgsolve, .options = furrr::furrr_options(seed = TRUE))
      }
    } else {
      function(.x) {
        purrr::imap_dfr(.x = .x, .f = sliceFunMrgsolve)
      }
    }

    results <- subdatasets %>% mapFun()

    # Tick progress
    if (!tick_slice) {
      progress <- progress %>% update_slice(subdatasets %>% length())
      progress <- progress %>% tick(tick_slice = tick_slice)
    }

    return(results %>% reorder_columns(dosing = dosing))
  }
)
