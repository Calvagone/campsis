#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

#' Simulate function.
#' 
#' @param model generic PMX model
#' @param dataset PMX dataset or 2-dimensional table
#' @param dest destination simulation engine, default is 'RxODE'
#' @param tablefun function or lambda formula to apply on exported 2-dimensional dataset
#' @param outvars variables to output in resulting dataframe
#' @param outfun function or lambda formula to apply on resulting dataframe after each replicate
#' @param seed seed value
#' @param replicates number of replicates, default is 1
#' @param ... optional arguments like declare
#' @return dataframe with all results
#' @export
simulate <- function(model, dataset, dest=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, ...) {
  stop("No default function is provided")
}

setGeneric("simulate", function(model, dataset, dest=NULL, tablefun=NULL, outvars=NULL, outfun=NULL, seed=NULL, replicates=1, ...) {
  dest <- if (is.null(dest)) "RxODE" else dest
  tablefun <- preprocessFunction(tablefun, "tablefun")
  outvars <- preprocessOutvars(outvars)
  outfun <- preprocessFunction(outfun, "outfun")
  seed <- getSeed(seed)
  replicates <- preprocessReplicates(replicates)
  standardGeneric("simulate")
})


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
exportTableDelegate <- function(model, dataset, dest, seed, tablefun) {
  if (is(dataset, "dataset")) {
    table <- dataset %>% export(dest=dest, model=model, seed=seed)
  } else {
    table <- dataset
  }
  table <- tablefun(table)
  return(table)
}

#' Simulation delegate.
#' 
#' @inheritParams simulate
#' @keywords internal
#' 
simulateDelegate <- function(model, dataset, dest, tablefun, outvars, outfun, seed, replicates, ...) {
  validObject(model)
  destEngine <- getSimulationEngineType(dest)
  if (replicates==1) {
    table <- exportTableDelegate(model=model, dataset=dataset, dest=dest, seed=seed, tablefun=tablefun)
    return(simulate(model=model, dataset=table, dest=destEngine, tablefun=tablefun,
                    outvars=outvars, outfun=outfun, seed=seed, replicates=replicates, ...))
  } else {
    # Get as many models as replicates
    setSeed(seed - 1) # Set seed before sampling parameters uncertainty
    models <- model %>% sample(replicates)
    
    # Run all models
    return(purrr::map2_df(.x=models, .y=seq_along(models), .f=function(model_, replicate) {
      seedInRep <- getSeedForReplicate(seed, replicate)
      table <- exportTableDelegate(model=model_, dataset=dataset, dest=dest, seed=seedInRep, tablefun=tablefun)
      return(simulate(model=model_, dataset=table, dest=destEngine, tablefun=tablefun,
                      outvars=outvars, outfun=outfun, seed=seedInRep, replicates=replicates, ...))
    }, .id="replicate"))
  }
}

setMethod("simulate", signature=c("pmx_model", "dataset", "character", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, tablefun, outvars, outfun, seed, replicates, ...) {
  iovNames <- dataset %>% getIOVNames()
  covariateNames <- dataset %>% getCovariateNames()
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, tablefun=tablefun,
                          outvars=outvars, outfun=outfun, seed=seed, replicates=replicates,
                          iovNames=iovNames, covariateNames=covariateNames, ...))
})


setMethod("simulate", signature=c("pmx_model", "data.frame", "character", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, tablefun, outvars, outfun, seed, replicates, ...) {
  return(simulateDelegate(model=model, dataset=dataset, dest=dest, tablefun=tablefun, outvars=outvars,
                          outfun=outfun, seed=seed, replicates=replicates, ...))
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
  out <- c("id", "time", "ARM", outvars_)
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
  
  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Slice number, put to 6, as the default number of threads in RxoDE
  # This number must correspond to the number of cores available
  # This should be set as static information (not in simulate method)
  # Note that mrgsolve does not seem to have parallelisation (to check)
  # It may also be interesting to parallelise the replicates (see later on)
  slices <- preprocessSlices(6, maxID=maxID)
  
  # Drop others 'argument'
  dropOthers <- dropOthers() %in% outvars

  # Extra argument declare (for mrgsolve only)
  user_declare <- processExtraArg(args, name="declare", mandatory=FALSE)
  iovNames <- processExtraArg(args, name="iovNames", mandatory=FALSE)
  covariateNames <- processExtraArg(args, name="covariateNames", mandatory=FALSE)
  declare <- unique(c(iovNames, covariateNames, user_declare, "ARM"))    

  # Export PMX model
  if (is(dest, "rxode_engine")) {
    engineModel <- model %>% pmxmod::export(dest="RxODE")
  } else if (is(dest, "mrgsolve_engine")) {
    outvars_ <- outvars[!(outvars %in% dropOthers())]
    outvars_ <- unique(c(outvars_, "ARM"))
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
              dropOthers=dropOthers, iovNames=iovNames, covariateNames=covariateNames))
}

setMethod("simulate", signature=c("pmx_model", "data.frame", "rxode_engine", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, tablefun, outvars, outfun, seed, replicates, ...) {
  
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
  covariatesToOutput <- outvars[outvars %in% c(config$covariateNames, colnames(rxmod@omega))]

  results <- eventsList %>% purrr::map_df(.f=function(events){
    tmp <- RxODE::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=events, returnType="tibble")
    
    # RxODE does not add the 'id' column if only 1 subject
    uniqueID <- unique(events$ID)
    if (length(uniqueID)==1) {
      tmp <- tmp %>% tibble::add_column(id=uniqueID, .before=1)
    }
    # Output covariates from outvars
    if (length(covariatesToOutput) > 0) {
      covs <- events %>% dplyr::select(dplyr::all_of(c("ID", covariatesToOutput))) %>% dplyr::rename(id=ID) %>% dplyr::distinct()
      if (nrow(covs)==length(uniqueID)) {
        tmp <- tmp %>% dplyr::left_join(covs, by="id")
      } else {
        warning("Covariates or ETA's cannot be merged.")
      }
    }
    return(processDropOthers(tmp, outvars=outvars, dropOthers=dropOthers))
  })
  return(outfun(results))
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "mrgsolve_engine", "function", "character", "function", "integer", "integer"),
          definition=function(model, dataset, dest, tablefun, outvars, outfun, seed, replicates, ...) {
  
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
    # Observation only set to TRUE to align results with RxODE
    tmp <- mod %>% mrgsolve::data_set(data=events) %>% mrgsolve::mrgsim(obsonly=TRUE, output="df")
    
    # Use same id and time columns as RxODE
    tmp <- tmp %>% dplyr::rename(id=ID, time=TIME)
    return(processDropOthers(tmp, outvars=outvars, dropOthers=dropOthers))
  })
  return(outfun(results))
})
