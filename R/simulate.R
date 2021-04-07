
#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

#' Get simulation engine type.
#' 
#' @param dest destination engine, string form
#' @return simulation engine type
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

setMethod("simulate", signature=c("pmx_model", "dataset", "character"), definition=function(model, dataset, dest, output=NULL, seed=NULL, slices=NULL, replicates=1, ...) {
  dest <- getSimulationEngineType(dest)
  originalSeed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates)
  
  if (replicates==1) {
    return(simulate(model=model, dataset=dataset, dest=dest, output=output, seed=originalSeed, slices=slices, ...))
  } else {
    setSeed(originalSeed - 1) # Set seed before sampling parameters uncertainty
    models <- model %>% sample(as.integer(replicates))
    return(purrr::map2_df(.x=models, .y=seq_along(models), .f=function(.x, .y) {
      return(simulate(model=.x, dataset=dataset, dest=dest, output=output, seed=getSeedForReplicate(originalSeed, .y), slices=slices, ...))
    }, .id="replicate"))
  }
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"rxode_engine"), definition=function(model, dataset, dest, seed, slices, ...) {
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed, ...)
  return(simulate(model=model, dataset=table, dest=dest, slices=slices, ...))
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"mrgsolve_engine"), definition=function(model, dataset, dest, seed, slices, ...) {
  table <- dataset %>% export(dest="mrgsolve", model=model, seed=seed, ...)
  
  # Variables to declare in the mrgsolve model
  iovNames <- dataset %>% getIOVNames()
  covariateNames <- dataset %>% getCovariateNames()
  declare <- c(iovNames, covariateNames)
  
  return(simulate(model=model, dataset=table, dest=dest, slices=slices, declare=declare, ...))
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "character"), definition=function(model, dataset, dest, output=NULL, seed=NULL, slices=NULL, replicates=1, ...) {
  dest <- getSimulationEngineType(dest)
  originalSeed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates)
  
  if (replicates==1) {
    return(simulate(model=model, dataset=dataset, dest=dest, output=output, slices=slices, ...))
  } else {
    setSeed(originalSeed - 1) # Set seed before sampling parameters uncertainty
    models <- model %>% sample(as.integer(replicates))
    return(purrr::map2_df(.x=models, .y=seq_along(models), .f=function(.x, .y) {
      return(simulate(model=.x, dataset=dataset, dest=dest, output=output, slices=slices, ...))
    }, .id="replicate"))
  }
})

#' Preprocess subjects ID's.
#' 
#' @param dataset current dataset, data frame form
#' @return list of consecutive ID's
#' @importFrom assertthat assert_that
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
#' @return updated model
#' @importFrom assertthat assert_that
#' 
preprocessArmColumn <- function(dataset, model) {
  if ("ARM" %in% colnames(dataset)) {
    pkRecord <- model@model %>% pmxmod::getByName("PK")
    pkRecord@code <- c(pkRecord@code, "ARM=ARM")
    model@model <- model@model %>% pmxmod::replace(pkRecord)
  }
  return(model)
}

#' Preprocess 'slices' argument.
#' 
#' @param slices slices argument corresponding to the number of subjects simulated at once
#' @return slices if not NULL, otherwise total number of subjects
#' @importFrom assertthat assert_that
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

#' Preprocess 'replicates' argument.
#' 
#' @param replicates number of replicates
#' @return same number
#' @importFrom assertthat assert_that
#' 
preprocessReplicates <- function(replicates) {
  assertthat::assert_that(is.numeric(replicates) && replicates%%1==0 && replicates > 0,
                          msg="replicates not a positive integer")
  return(replicates)
}

#' Preprocess 'output' argument. Output is either the columns to keep from the 
#' returned dataframe or the function to apply on this dataframe.
#'
#' @param output character vector or function
#' @return output
#' @importFrom assertthat assert_that
#'
preprocessOutput <- function(output) {
  assertthat::assert_that(is.null(output) || is.character(output) || is.function(output),
                          msg=paste0("output must be a character vector with the column names to keep",
                                     "or a function to apply on the returned dataframe"))
  return(output)
}

#' Process the data frame returned by mrgsolve or RxODE.
#'
#' @param x the current data frame
#' @param output character vector or function
#' @return processed data frame
#' @importFrom dplyr all_of select
#'
processReturnedDataframe <- function(x, output) {
  if (is.null(output)) {
    return(x)
  }
  if (is.function(output)) {
    return(output(x))
  } else {
    return(x %>% dplyr::select(dplyr::all_of(output)))
  }
}

#' Preprocess arguments of the simulate method.
#' 
#' @param model PMX model
#' @param dataset dataset, data.frame form
#' @param dest destination engine
#' @param slices number of subjects per slice
#' @return a simulation configuration
#' @importFrom purrr map2
#' 
preprocessSimulateArguments <- function(model, dataset, dest, output, slices, ...) {
  # Check extra arguments
  args <- list(...)
  
  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Slice number
  slices <- preprocessSlices(slices, maxID=maxID)
  
  # Output argument
  output <- preprocessOutput(output)
  
  # Variables to declare (mrgsolve only)
  declare <- processExtraArg(args, name="declare")
  
  # Export PMX model
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>% pmxmod::export(dest="RxODE")
  } else if (is(dest, "mrgsolve_engine")) {
    engineModel <- model %>% pmxmod::export(dest="mrgsolve")
  }
  
  # Compute all slice rounds to perform
  sliceRounds <- list(start=seq(1, maxID, by=slices), end=seq(0, maxID-1, by=slices) + slices)
  
  # Prepare list of events (1 event dataframe per slice/round)
  eventsList <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
    events <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
    return(events)
  })
  
  return(list(slices=slices, output=output, declare=declare,
              engineModel=engineModel, eventsList=eventsList))
}

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"rxode_engine"), definition=function(model, dataset, dest, slices, ...) {
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  
  # Retrieve simulation config
  config <- preprocessSimulateArguments(model=model, dataset=dataset, dest=dest, slices=slices, ...)

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
  output <- config$output
  results <- eventsList %>% purrr::map_df(.f=function(events){
    tmp <- RxODE::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=events, returnType="tibble")
    # RxODE does not add the 'id' column if only 1 subject
    uniqueID <- unique(events$ID)
    if (length(uniqueID)==1) {
      tmp <- tmp %>% dplyr::mutate(id=uniqueID)
    }
    return(processReturnedDataframe(tmp, output=output))
  })
  return(results)
})

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"mrgsolve_engine"), definition=function(model, dataset, dest, slices, ...) {
  
  # Retrieve simulation config
  config <- preprocessSimulateArguments(model=model, dataset=dataset, dest=dest, slices=slices, ...)
  
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
  for (variable in config$declare) {
    mrgmod@param <- mrgmod@param %>% append(paste0(variable, " : ", 0, " : ", variable))
  }
  
  # Add ARM equation in model
  hasARM <- "ARM" %in% colnames(dataset)
  
  if (hasARM) {
    mrgmod@param <- mrgmod@param %>% append("ARM : 0 : ARM")
    mrgmod@table <- mrgmod@table %>% append("capture ARM=ARM;") 
  }
  
  # Instantiate mrgsolve model
  mod <- mrgsolve::mcode("model", mrgmod %>% pmxmod::toString())
  
  # Launch mrgsolve
  eventsList <- config$eventsList
  output <- config$output
  results <- eventsList %>% purrr::map_df(.f=function(events){
    # Observation only set to TRUE to align results with RxODE
    tmp <- mod %>% mrgsolve::data_set(data=events) %>% mrgsolve::mrgsim(obsonly=TRUE, output="df")
    
    # Use same id and time columns as RxODE
    tmp <- tmp %>% dplyr::rename(id=ID, time=TIME)

    return(processReturnedDataframe(tmp, output=output))
  })
  return(results)
})
