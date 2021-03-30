
#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

setMethod("simulate", signature=c("pmx_model", "dataset", "character"), definition=function(model, dataset, dest, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_engine"), ...))
  } else if (dest=="mrgsolve") {
    return(simulate(model=model, dataset=dataset, dest=new("mrgsolve_engine"), ...))
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "character"), definition=function(model, dataset, dest, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_engine"), ...))
  } else if (dest=="mrgsolve") {
    return(simulate(model=model, dataset=dataset, dest=new("mrgsolve_engine"), ...))
  } else {
    stop("Only RxODE and mrgsolve are supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"rxode_engine"), definition=function(model, dataset, dest, ...) {

  # Export to data frame
  table <- dataset %>% export(dest="RxODE", model=model, ...)
  
  return(simulate(model=model, dataset=table, dest=dest, ...))
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"mrgsolve_engine"), definition=function(model, dataset, dest, ...) {
  
  # Export to data frame (data frame RxODE = data frame mrgsolve)
  table <- dataset %>% export(dest="RxODE", model=model, ...)
  
  return(simulate(model=model, dataset=table, dest=dest, ...))
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

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"rxode_engine"), definition=function(model, dataset, dest, ...) {
    # Check extra arguments
    args <- list(...)

    # IDs
    ids <- preprocessIds(dataset)
    maxID <- max(ids)
    
    # Add ARM equation in model
    model <- preprocessArmColumn(dataset, model)
    
    # Slice number
    slices <- processExtraArg(args, name="slices", default=maxID)

    # Output variables
    output <- processExtraArg(args, name="output")
    
    # Export PMX model to RxODE
    rxmod <- model %>% pmxmod::export(dest="RxODE")

    # Compute all slice rounds to perform
    sliceRounds <- list(start=seq(1, maxID, by=slices), end=seq(0, maxID-1, by=slices) + slices)
    
    # Prepare list of events (1 event dataframe per slice/round)
    eventsList <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
      events <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
      return(events)
    })
    
    # Instantiate RxODE model
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

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"mrgsolve_engine"), definition=function(model, dataset, dest, ...) {
  # Check extra arguments
  args <- list(...)
  
  # IDs
  ids <- preprocessIds(dataset)
  maxID <- max(ids)
  
  # Add ARM equation in model
  model <- preprocessArmColumn(dataset, model)
  
  # Slice number
  slices <- processExtraArg(args, name="slices", default=maxID)
  
  # Output variables
  output <- processExtraArg(args, name="output")
  
  # Export PMX model to RxODE
  mrgmod <- model %>% pmxmod::export(dest="mrgsolve")
  
  # Disable IIV in mrgsolve model
  mrgmod@omega <- character(0) # IIV managed by pmxsim
  
  # Declare all ETA's in the PARAM block
  omegas <- rxodeMatrix(model, )
  for (omega in (model@parameters %>% pmxmod::select("omega"))@list) {
    if(omega %>% isDiag()) {
      etaName <- omega %>% pmxmod::getNameInModel()
      mrgmod@param <- mrgmod@param %>% append(paste0(etaName, " : ", 0, " : ", etaName))
    }
  }
  
  # Compute all slice rounds to perform
  sliceRounds <- list(start=seq(1, maxID, by=slices), end=seq(0, maxID-1, by=slices) + slices)
  
  # Prepare list of events (1 event dataframe per slice/round)
  eventsList <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
    events <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
    return(events)
  })
  
  # Instantiate mrgsolve model
  mod <- mrgsolve::mcode("model", mrgmod %>% pmxmod::toString())
  
  # Launch mrgsolve
  results <- eventsList %>% purrr::map_df(.f=function(events){
    tmp <- (mod %>% mrgsolve::data_set(data=events) %>% mrgsolve::mrgsim())@data
    if (!is.null(output)) {
      tmp <- tmp %>% dplyr::select(dplyr::all_of(output))
    }
    # Use same id and time columns as RxODE
    tmp <- tmp %>% dplyr::rename(id=ID, time=TIME)
    # Mrgsolve also keeps values at EVID=1 -> get rid of duplicates in dataset
    # Note: add argument in simulate function?
    tmp <- tmp %>% dplyr::distinct(dplyr::across(c(id, time)), .keep_all=TRUE)
    return(tmp)
  })
  return(results)
})
