
#_______________________________________________________________________________
#----                             simulate                                  ----
#_______________________________________________________________________________

setMethod("simulate", signature=c("pmx_model", "dataset", "character"), definition=function(model, dataset, dest, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_type"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "data.frame", "character"), definition=function(model, dataset, dest, ...) {
  if (dest=="RxODE") {
    return(simulate(model=model, dataset=dataset, dest=new("rxode_type"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("simulate", signature=c("pmx_model", "dataset" ,"rxode_type"), definition=function(model, dataset, dest, ...) {
  
  config <- new("config", default_depot_cmt=as.integer(1), default_obs_cmt=as.integer(2))
  dataset <- dataset %>% export(dest="RxODE", config=config)
  
  return(simulate(model=model, dataset=dataset, dest=dest, ...))
})

setMethod("simulate", signature=c("pmx_model", "data.frame" ,"rxode_type"), definition=function(model, dataset, dest, ...) {
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

    # Compute all rounds to perform
    rounds <- list(start=seq(1, maxID, by=sliceNo), end=seq(0, maxID-1, by=sliceNo) + sliceNo)

    # Export PMX model to RxODE
    rxmod <- model %>% pmxmod::export(dest="RxODE")
    
    # Launch RxODE
    results <- purrr::map2_df(rounds$start, rounds$end, .f=function(.x, .y){
      events <- dataset %>% dplyr::filter(ID >= .x & ID <= .y)
      tmp <- RxODE::rxSolve(paste0(rxmod@code, collapse="\n"), params=rxmod@theta, omega=rxmod@omega, sigma=rxmod@sigma,
                     events=events, returnType="tibble")
      if (!is.null(output)) {
        tmp <- tmp %>% dplyr::select(output)
      }
      return(tmp)
    })
  return(results)
})