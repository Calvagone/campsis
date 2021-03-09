
#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

setMethod("simulate", signature=c("pmx_model", "dataset", "character"), definition=function(model, dataset, dest, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_engine"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "character"), definition=function(model, dataset, dest, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_engine"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"rxode_engine"), definition=function(model, dataset, dest, ...) {
  
  dataset <- dataset %>% export(dest="RxODE")
  
  return(simulate(model=model, dataset=dataset, dest=dest, ...))
})

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"rxode_engine"), definition=function(model, dataset, dest, ...) {
    # Check extra arguments
    args <- list(...)
    
    # IDs
    ids <- unique(dataset$ID)
    
    maxID <- max(ids)
    assertthat::assert_that(all(ids==seq_len(maxID)), msg="ID's must be consecutive numbers, starting at 1")
    
    # Slice number
    if (hasName(args, "sliceNo")) {
      sliceNo <- args$sliceNo
    } else {
      sliceNo <- maxID
    }
    
    # Output variables
    if (hasName(args, "output")) {
      output <- args$output
    } else {
      output <- NULL
    }

    # Compute all slice rounds to perform
    sliceRounds <- list(start=seq(1, maxID, by=sliceNo), end=seq(0, maxID-1, by=sliceNo) + sliceNo)
    
    # Prepare list of events (1 event dataframe per slice/round)
    eventsList <- purrr::map2(sliceRounds$start, sliceRounds$end, .f=function(.x, .y){
      events <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
      return(events)
    })

    # Export PMX model to RxODE
    rxmod <- model %>% pmxmod::export(dest="RxODE")
    
    # Instantiate RxODE model
    mod <- RxODE::RxODE(paste0(rxmod@code, collapse="\n"))
    
    # Preparing parameters
    params <- rxmod@theta
    omega <- rxmod@omega
    sigma <- rxmod@sigma
    if (nrow(omega)==0) {
      omega <- NULL
    }
    if (nrow(sigma)==0) {
      sigma <- NULL
    }
    
    # Launch RxODE
    results <- eventsList %>% purrr::map_df(.f=function(events){
      tmp <- RxODE::rxSolve(object=mod, params=params, omega=omega, sigma=sigma, events=events, returnType="tibble")
      if (!is.null(output)) {
        tmp <- tmp %>% dplyr::select(dplyr::all_of(output))
      }
      return(tmp)
    })
  return(results)
})
