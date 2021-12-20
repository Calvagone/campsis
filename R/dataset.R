
#_______________________________________________________________________________
#----                         dataset class                                 ----
#_______________________________________________________________________________

#' 
#' Dataset class.
#' 
#' @slot arms a list of treatment arms
#' @slot config dataset configuration for export
#' @slot iiv data frame containing the inter-individual variability (all ETAS) for the export
#' @export
setClass(
  "dataset",
  representation(
    arms = "arms",
    config = "dataset_config",
    iiv = "data.frame"
  ),
  prototype=prototype(arms=new("arms"), config=DatasetConfig(), iiv=data.frame())
)

#'
#' Create a dataset.
#'
#' @param subjects number of subjects in the default arm
#' @return a dataset
#' @export
Dataset <- function(subjects=NULL) {
  arms=new("arms")
  if (!is.null(subjects)) {
    arm <- arms %>% default()
    arm@subjects <- as.integer(subjects)
    arms <- arms %>% add(arm)
  }
  return(new("dataset", arms=arms))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

createDefaultArmIfNotExists <- function(object) {
  # Get default arm
  arm <- object@arms %>% default()
  
  # Add it if not yet added to list
  if (object@arms %>% length() == 0) {
    object@arms <- object@arms %>% add(arm)
  }
  return(object)
}

setMethod("add", signature = c("dataset", "list"), definition = function(object, x) {
  for (element in x) {
    object <- object %>% add(element)
  }
  return(object)
})

setMethod("add", signature = c("dataset", "arm"), definition = function(object, x) {
  object@arms <- object@arms %>% add(x) 
  return(object)
})

setMethod("add", signature = c("dataset", "pmx_element"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm <- arm %>% add(x)
  object@arms <- object@arms %>% replace(arm)
  return(object)
})

setMethod("add", signature = c("dataset", "dataset_config"), definition = function(object, x) {
  object@config <- x
  return(object)
})

#_______________________________________________________________________________
#----                           contains                                    ----
#_______________________________________________________________________________

setMethod("contains", signature = c("dataset", "pmx_element"), definition = function(object, x) {
  if (object@arms %>% length() == 0) {
    return(FALSE)
  }
  return(object@arms@list %>% purrr::map_lgl(~.x %>% contains(x)) %>% any())
})

#_______________________________________________________________________________
#----                              delete                                   ----
#_______________________________________________________________________________

setMethod("delete", signature = c("dataset", "pmx_element"), definition = function(object, x) {
  object@arms@list <- object@arms@list %>% purrr::map(~.x %>% delete(x))
  return(object)
})

#_______________________________________________________________________________
#----                               find                                    ----
#_______________________________________________________________________________

setMethod("find", signature = c("dataset", "pmx_element"), definition = function(object, x) {
  elements <- object@arms@list %>% purrr::map(~.x %>% find(x))
  if (!is.null(elements)) {
    elements <- elements[[1]] # Return first element in all cases
  }
  return(elements)
})

#_______________________________________________________________________________
#----                           getCovariates                               ----
#_______________________________________________________________________________

#' @rdname getCovariates
setMethod("getCovariates", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getCovariates())
})

#_______________________________________________________________________________
#----                         getEventCovariates                            ----
#_______________________________________________________________________________

#' @rdname getEventCovariates
setMethod("getEventCovariates", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getEventCovariates())
})

#_______________________________________________________________________________
#----                         getFixedCovariates                            ----
#_______________________________________________________________________________

#' @rdname getFixedCovariates
setMethod("getFixedCovariates", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getFixedCovariates())
})

#_______________________________________________________________________________
#----                       getTimeVaryingCovariates                        ----
#_______________________________________________________________________________

#' @rdname getTimeVaryingCovariates
setMethod("getTimeVaryingCovariates", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getTimeVaryingCovariates())
})

#_______________________________________________________________________________
#----                              getIOVs                                  ----
#_______________________________________________________________________________

#' @rdname getIOVs
setMethod("getIOVs", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getIOVs())
})

#_______________________________________________________________________________
#----                            getOccasions                               ----
#_______________________________________________________________________________

#' @rdname getOccasions
setMethod("getOccasions", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getOccasions())
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

#' @rdname getTimes
setMethod("getTimes", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getTimes())
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the number of subjects contained in this dataset.
#' 
#' @param x dataset
#' @return a number
setMethod("length", signature=c("dataset"), definition=function(x) {
  subjectsPerArm <- x@arms@list %>% purrr::map_int(.f=~.x@subjects) 
  return(sum(subjectsPerArm))
})

#_______________________________________________________________________________
#----                             replace                                   ----
#_______________________________________________________________________________

setMethod("replace", signature=c("dataset", "arm"), definition=function(object, x) {
  object@arms <- object@arms %>% replace(x)
  return(object)
})

setMethod("replace", signature = c("dataset", "pmx_element"), definition = function(object, x) {
  object@arms@list <- object@arms@list %>% purrr::map(~.x %>% replace(x))
  return(object)
})

#_______________________________________________________________________________
#----                           setSubjects                                 ----
#_______________________________________________________________________________

#' @rdname setSubjects
setMethod("setSubjects", signature = c("dataset", "integer"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@subjects <- x
  object <- object %>% replace(arm)
  validObject(object)
  return(object)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Generate IIV.
#' 
#' @param omega omega matrix
#' @param n number of subjects
#' @return IIV data frame
#' @export
generateIIV <- function(omega, n) {
  if (nrow(omega)==0) {
    return(data.frame())
  }
  iiv <- MASS::mvrnorm(n=n, mu=rep(0, nrow(omega)), Sigma=omega)
  if (n==1) {
    iiv <- t(iiv) # If n=1, mvrnorm result is a numeric vector, not a matrix
  }
  iiv <- iiv %>% as.data.frame()
  return(iiv)
}

#' Process distribution-derived objects as covariates.
#' 
#' @param list list of distribution-derived objects
#' @return a list of covariates that can be easily processed
#' @keywords internal
#' 
processAsCovariate <- function(list) {
  covariates <- new("covariates")
  for (distribution in list) {
    dist <- distribution@distribution
    if (is(dist, "sampled_distribution")) {
      name <- distribution %>% getColumnName()
      covariates <- covariates %>% add(Covariate(name=name, distribution=dist))
    } else {
      stop(paste0("Unknown distribution class: ", as.character(class(dist))))
    }
  }
  return(covariates)
}

#' Sample covariates list.
#' 
#' @param covariates list of covariates to sample
#' @param n number of desired samples
#' @return a dataframe of n rows, 1 column per covariate
#' @keywords internal
#' 
sampleCovariatesList <- function(covariates, n) {
  retValue <- covariates@list %>% purrr::map_dfc(.f=function(covariate) {
    sampleDistributionAsTibble(covariate@distribution, n=n, colname=covariate@name)
  })
  return(retValue)
}

#' Sample a distribution and return a tibble.
#' 
#' @param distribution any distribution
#' @param n number of desired samples
#' @param colname name of the unique column in tibble
#' @return a tibble of n rows and 1 column
#' @keywords internal
#'
sampleDistributionAsTibble <- function(distribution, n, colname) {
  return(tibble::tibble(!!colname := (distribution %>% sample(n=n))@sampled_values))
}

#' Apply compartment characteristics from model.
#' In practice, only compartment infusion duration needs to be applied.
#' 
#' @param table current dataset
#' @param properties compartment properties from model
#' @return updated dataset
#' @importFrom dplyr mutate
#' 
applyCompartmentCharacteristics <- function(table, properties) {
  for (property in properties@list) {
    isInfusion <- is(property, "compartment_infusion_duration")
    isRate <- is(property, "compartment_infusion_rate")
    if (isInfusion || isRate) {
      compartment <- property@compartment
      if (!("RATE" %in% colnames(table))) {
        table <- table %>% dplyr::mutate(RATE=0)
      }
      rateValue <- ifelse(isRate, -1, -2)
      table <- table %>% dplyr::mutate(RATE=ifelse(EVID==1 & CMT==compartment & IS_INFUSION %in% TRUE, rateValue, RATE))
    }
  }
  return(table)
}

setMethod("export", signature=c("dataset", "character"), definition=function(object, dest, seed=NULL, nocb=FALSE, event_related_column=FALSE, ...) {
  destinationEngine <- getSimulationEngineType(dest)
  table <- object %>% export(destinationEngine, seed=seed, nocb=nocb, ...)
  if (!event_related_column) {
    table <- table %>% dplyr::select(-EVENT_RELATED)
  }
  return(table)
})

#' Export delegate method. This method is common to RxODE and mrgsolve.
#' 
#' @param object current dataset
#' @param dest destination engine
#' @param seed seed value
#' @param nocb nocb value, logical value
#' @param ... extra arguments
#' @return 2-dimensional dataset, same for RxODE and mrgsolve
#' @importFrom dplyr arrange bind_rows left_join
#' @importFrom campsismod export
#' @importFrom tibble add_column tibble
#' @importFrom purrr accumulate map_df map_int map2_df
#' @importFrom rlang parse_expr
#' @keywords internal
#' 
exportDelegate <- function(object, dest, seed, nocb, ...) {
  args <- list(...)
  model <- args$model
  if (!is.null(model) && !is(model, "campsis_model")) {
    stop("Please provide a valid PMX model.")
  }
  
  # Set seed value
  setSeed(getSeed(seed))
  
  # Generate IIV only if model is provided
  if (is.null(model)) {
    iiv <- data.frame()
  } else {
    rxmod <- model %>% export(dest="RxODE")
    subjects <- object %>% length()
    iiv <- generateIIV(omega=rxmod@omega, n=subjects)
    if (nrow(iiv) > 0) {
      iiv <- iiv %>% tibble::add_column(ID=seq_len(subjects), .before=1)
    }
  }
  
  # Retrieve dataset configuration
  config <- object@config
  
  # Use either arms or default_arm
  arms <- object@arms
  if (length(arms) == 0) {
    stop("No entry in dataset. Not able to export anything...")
  }
  
  # Compute max ID per arm
  maxIDPerArm <- arms@list %>% purrr::map_int(~.x@subjects) %>% purrr::accumulate(~(.x+.y))
  
  retValue <- purrr::map2_df(arms@list, maxIDPerArm, .f=function(arm, maxID) {
    armID <- arm@id
    subjects <- arm@subjects
    protocol <- arm@protocol
    treatment <- protocol@treatment %>% assignDoseNumber()
    if (treatment %>% length() > 0) {
      maxDoseNumber <- (treatment@list[[treatment %>% length()]])@dose_number
    } else { 
      maxDoseNumber <- 1 # Default
    }
    observations <- protocol@observations
    covariates <- arm@covariates
    timeVaryingCovariates <- covariates %>% campsismod::select("time_varying_covariate")
    treatmentIovs <- treatment@iovs
    occasions <- treatment@occasions
    doseAdaptations <- treatment@dose_adaptations
    
    # Generating subject ID's
    ids <- seq_len(subjects) + maxID - subjects
    
    # Create the base table with all treatment entries and observations
    needsDV <- observations@list %>% purrr::map_lgl(~.x@dv %>% length() > 0) %>% any()
    table <- c(treatment@list, observations@list) %>% purrr::map_df(.f=~sample(.x, n=subjects, ids=ids, config=config, armID=armID, needsDV=needsDV))
    table <- table %>% dplyr::arrange(ID, TIME, EVID)

    # Sampling covariates
    cov <- sampleCovariatesList(covariates, n=length(ids))
    if (nrow(cov) > 0) {
      # Left join "fixed" covariates
      cov <- cov %>% tibble::add_column(ID=ids, .before=1)
      table <- table %>% dplyr::left_join(cov, by="ID")
      
      # Retrieve time-varying covariate names
      timeVaryingCovariateNames <- timeVaryingCovariates %>% getNames()
      
      # Merge time-varying covariate names
      if (timeVaryingCovariateNames %>% length() > 0) {
        # Only keep first row. Please note that NA's will be filled in 
        # by the final export method (depending on variables nocb & nocbvars)
        table <- table %>% dplyr::group_by(ID) %>%
          dplyr::mutate_at(.vars=timeVaryingCovariateNames, .funs=~ifelse(dplyr::row_number()==1, .x, as.numeric(NA))) %>%
          dplyr::ungroup()
        
        # Merge all time varying covariate tables into a single table
        # The idea is to use 1 EVID=2 row per subject time
        timeCov <- mergeTimeVaryingCovariates(timeVaryingCovariates, ids) %>%
          sampleTimeVaryingCovariates(armID=armID, needsDV=needsDV)
        
        # Bind with treatment and observations and sort
        table <- dplyr::bind_rows(table, timeCov)
        table <- table %>% dplyr::arrange(ID, TIME, EVID)
      }  
    }
    
    # Sampling IOV's
    for (treatmentIov in treatmentIovs@list) {
      doseNumbers <- treatmentIov@dose_numbers
      doseNumbers <- if (doseNumbers %>% length()==0) {seq_len(maxDoseNumber)} else {doseNumbers}
      iov <- sampleDistributionAsTibble(treatmentIov@distribution, n=length(ids)*length(doseNumbers), colname=treatmentIov@colname)
      iov <- iov %>% dplyr::mutate(ID=rep(ids, each=length(doseNumbers)), DOSENO=rep(doseNumbers, length(ids)))
      table <- table %>% dplyr::left_join(iov, by=c("ID","DOSENO"))
    }

    # Joining IIV
    if (nrow(iiv) > 0) {
      table <- table %>% dplyr::left_join(iiv, by="ID")
    }
    
    # Joining occasions
    for (occasion in occasions@list) {
      occ <- tibble::tibble(DOSENO=occasion@dose_numbers, !!occasion@colname:=occasion@values)
      table <- table %>% dplyr::left_join(occ, by="DOSENO")
    }
    
    # Apply formula if dose adaptations are present
    for (doseAdaptation in doseAdaptations@list) {
      compartments <- doseAdaptation@compartments
      expr <- rlang::parse_expr(doseAdaptation@formula)
      if (compartments %>% length() > 0) {
        table <- table %>% dplyr::mutate(AMT=ifelse(CMT %in% compartments, eval(expr), AMT))
      } else {
        table <- table %>% dplyr::mutate(AMT=eval(expr))
      }
    }
    
    return(table)
  })
  
  # Apply compartment properties coming from the model
  if (!is.null(model)) {
    retValue <- applyCompartmentCharacteristics(retValue, model@compartments@properties)
  }
  
  # Remove IS_INFUSION column
  retValue <- retValue %>% dplyr::select(-IS_INFUSION)
  
  return(retValue)
}

#' Fill IOV/Occasion columns.
#' 
#' Problem in RxODE (LOCF mode) / mrgsolve (LOCF mode), if 2 rows have the same time (often: OBS then DOSE), first row covariate value is taken!
#' Workaround: identify these rows (group by ID and TIME) and apply a fill in the UP direction.
#' 
#' @param table current table
#' @param columnNames the column names to fill
#' @param downDirectionFirst TRUE: first fill down then fill up (by ID & TIME). FALSE: First fill up (by ID & TIME), then fill down
#' @return 2-dimensional dataset
#' @importFrom dplyr all_of group_by mutate_at
#' @importFrom tidyr fill
#' @keywords internal
#' 
fillIOVOccColumns <- function(table, columnNames, downDirectionFirst) {
  if (downDirectionFirst) {
    table <- table %>% dplyr::group_by(ID) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="down")           # 1
    table <- table %>% dplyr::group_by(ID, TIME) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="up")       # 2
    table <- table %>% dplyr::group_by(ID) %>% dplyr::mutate_at(.vars=columnNames, .funs=~ifelse(is.na(.x), 0, .x)) # 3
  } else {
    table <- table %>% dplyr::group_by(ID, TIME) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="up")       # 2
    table <- table %>% dplyr::group_by(ID) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="down")           # 1
    table <- table %>% dplyr::group_by(ID) %>% dplyr::mutate_at(.vars=columnNames, .funs=~ifelse(is.na(.x), 0, .x)) # 3
  }
  return(table)
}

#' Counter-balance NOCB mode for occasions & IOV.
#' This function will simply shift all the related occasion & IOV columns to the right (by one).
#' 
#' @param table current table
#' @param columnNames columns to be counter-balanced
#' @return 2-dimensional dataset
#' @importFrom dplyr group_by mutate_at n
#' @keywords internal
#'
counterBalanceNocbMode <- function(table, columnNames) {
  return(table %>% dplyr::group_by(ID) %>% dplyr::mutate_at(.vars=columnNames, .funs=~c(.x[1], .x[-dplyr::n()])))
}

#' Counter-balance LOCF mode for occasions & IOV.
#' This function will simply shift all the related occasion & IOV columns to the left (by one).
#' 
#' @param table current table
#' @param columnNames columns to be counter-balanced
#' @return 2-dimensional dataset
#' @importFrom dplyr group_by mutate_at n
#' @keywords internal
#'
counterBalanceLocfMode <- function(table, columnNames) {
  return(table %>% dplyr::group_by(ID) %>% dplyr::mutate_at(.vars=columnNames, .funs=~c(.x[-1], .x[dplyr::n()])))
}

setMethod("export", signature=c("dataset", "rxode_engine"), definition=function(object, dest, seed, ...) {
  
  table <- exportDelegate(object=object, dest=dest, seed=seed, ...)
  nocb <- campsismod::processExtraArg(list(...), "nocb", default=FALSE)
  nocbvars <- campsismod::processExtraArg(list(...), "nocbvars", default=NULL)

  # IOV / Occasion / Time-varying covariates post-processing
  iovOccNames <- c(object %>% getIOVs() %>% getNames(),
                   object %>% getOccasions() %>% getNames(),
                   object %>% getTimeVaryingCovariates() %>% getNames())
  iovOccNamesNocb <- iovOccNames[iovOccNames %in% nocbvars]
  iovOccNamesLocf <- iovOccNames[!(iovOccNames %in% nocbvars)]
  
  if (nocb) {
    table <- fillIOVOccColumns(table, columnNames=iovOccNamesNocb, downDirectionFirst=TRUE)
    table <- fillIOVOccColumns(table, columnNames=iovOccNamesLocf, downDirectionFirst=FALSE)
    table <- counterBalanceNocbMode(table, columnNames=iovOccNamesNocb)
  } else {
    table <- fillIOVOccColumns(table, columnNames=iovOccNames, downDirectionFirst=FALSE)
  }
  
  return(table %>% dplyr::ungroup())
})

setMethod("export", signature=c("dataset", "mrgsolve_engine"), definition=function(object, dest, seed, ...) {
  
  table <- exportDelegate(object=object, dest=dest, seed=seed, ...)
  nocb <- campsismod::processExtraArg(list(...), "nocb", default=FALSE)
  nocbvars <- campsismod::processExtraArg(list(...), "nocbvars",  default=NULL)
  
  # IOV / Occasion / Time-varying covariates post-processing
  iovOccNames <- c(object %>% getIOVs() %>% getNames(),
                   object %>% getOccasions() %>% getNames(),
                   object %>% getTimeVaryingCovariates() %>% getNames())
  iovOccNamesNocb <- iovOccNames[iovOccNames %in% nocbvars]
  iovOccNamesLocf <- iovOccNames[!(iovOccNames %in% nocbvars)]
  
  if (nocb) {
    table <- fillIOVOccColumns(table, columnNames=iovOccNames, downDirectionFirst=FALSE) # TRUE/FALSE not important (like NONMEM)
    table <- counterBalanceNocbMode(table, columnNames=iovOccNamesNocb)
  } else {
    table <- fillIOVOccColumns(table, columnNames=iovOccNames, downDirectionFirst=FALSE)
    table <- counterBalanceLocfMode(table, columnNames=iovOccNamesLocf)
  }
  
  return(table %>% dplyr::ungroup())
})
