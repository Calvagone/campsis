
#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

setMethod("simulate", signature=c("pmx_model", "dataset", "character"), definition=function(model, dataset, dest, slices=NULL, replicates=1, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_engine"), slices=slices, replicates=replicates, ...))
  } else if (dest=="mrgsolve") {
    return(simulate(model=model, dataset=dataset, dest=new("mrgsolve_engine"), slices=slices, replicates=replicates, ...))
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "character"), definition=function(model, dataset, dest, slices=NULL, replicates=1, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_engine"), slices=slices, replicates=replicates, ...))
  } else if (dest=="mrgsolve") {
    return(simulate(model=model, dataset=dataset, dest=new("mrgsolve_engine"), slices=slices, replicates=replicates, ...))
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"rxode_engine"), definition=function(model, dataset, dest, slices, replicates, ...) {

  # Export to data frame
  table <- dataset %>% export(dest="RxODE", model=model, ...)
  
  return(simulate(model=model, dataset=table, dest=dest, slices=slices, replicates=replicates, ...))
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"mrgsolve_engine"), definition=function(model, dataset, dest, slices, replicates, ...) {
  
  # Export to data frame (data frame RxODE = data frame mrgsolve)
  table <- dataset %>% export(dest="RxODE", model=model, ...)
  
  # Variables to declare in the mrgsolve model
  iovNames <- dataset %>% getIOVNames()
  covariateNames <- dataset %>% getCovariateNames()
  declare <- c(iovNames, covariateNames)
  
  # Mrgsolve complains if treatment IOV has NA's for observations
  # Warning: Parameter column IOV_KA must not contain missing values
  for (iovName in iovNames) {
    table <- table %>% dplyr::group_by(ID) %>% tidyr::fill(dplyr::all_of(iovName), .direction="downup")
  }
  
  return(simulate(model=model, dataset=table, dest=dest, declare=declare, slices=slices, replicates=replicates, ...))
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

#' Preprocess arguments of the simulate method.
#' 
#' @param model PMX model
#' @param dataset dataset, data.frame form
#' @param dest destination engine
#' @param slices number of subjects per slice
#' @param replicates number of replicates to simulate
#' @return a simulation configuration
#' @importFrom purrr map2
#' 
preprocessSimulateArguments <- function(model, dataset, dest, slices, replicates, ...) {
  # Check extra arguments
  args <- list(...)
  
  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Slice number
  slices <- preprocessSlices(slices, maxID=maxID)
  
  # Replicates number
  replicates <- preprocessReplicates(replicates)
  
  # Output variables
  output <- processExtraArg(args, name="output")
  
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
  
  return(list(slices=slices, replicates=replicates, output=output, declare=declare,
              engineModel=engineModel, eventsList=eventsList))
}

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"rxode_engine"), definition=function(model, dataset, dest, slices, replicates, ...) {
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  
  # Retrieve simulation config
  config <- preprocessSimulateArguments(model=model, dataset=dataset, dest=dest, slices=slices, replicates=replicates, ...)

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
    if (!is.null(output)) {
      tmp <- tmp %>% dplyr::select(dplyr::all_of(output))
    }
    # RxODE does not add the 'id' column if only 1 subject
    uniqueID <- unique(events$ID)
    if (length(uniqueID) == 1) {
      tmp <- tmp %>% dplyr::mutate(id=uniqueID)
    }
    return(tmp)
  })
  return(results)
})

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"mrgsolve_engine"), definition=function(model, dataset, dest, slices, replicates, ...) {
  
  # Retrieve simulation config
  config <- preprocessSimulateArguments(model=model, dataset=dataset, dest=dest, slices=slices, replicates=replicates, ...)
  
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
    if (!is.null(output)) {
      tmp <- tmp %>% dplyr::select(dplyr::all_of(output))
    }
    # Use same id and time columns as RxODE
    tmp <- tmp %>% dplyr::rename(id=ID, time=TIME)
    return(tmp)
  })
  return(results)
})
