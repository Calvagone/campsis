
#_______________________________________________________________________________
#----                         dataset class                                 ----
#_______________________________________________________________________________

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

setMethod("add", signature = c("dataset", "treatment_entry"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@treatment <- arm@protocol@treatment %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
})

setMethod("add", signature = c("dataset", "treatment_iov"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@treatment <- arm@protocol@treatment %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
})

setMethod("add", signature = c("dataset", "observations"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@observations <- arm@protocol@observations %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
})

setMethod("add", signature = c("dataset", "covariate"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@covariates <- arm@covariates %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
})

setMethod("add", signature = c("dataset", "dataset_config"), definition = function(object, x) {
  object@config <- x
  return(object)
})

#_______________________________________________________________________________
#----                          getCovariateNames                            ----
#_______________________________________________________________________________

setMethod("getCovariateNames", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getCovariateNames())
})

#_______________________________________________________________________________
#----                            getIOVNames                                ----
#_______________________________________________________________________________

setMethod("getIOVNames", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getIOVNames())
})

#_______________________________________________________________________________
#----                         getOccasionNames                              ----
#_______________________________________________________________________________

setMethod("getOccasionNames", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getOccasionNames())
})

#_______________________________________________________________________________
#----                     getTimeVaryingCovariateNames                      ----
#_______________________________________________________________________________

setMethod("getTimeVaryingCovariateNames", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getTimeVaryingCovariateNames())
})

#_______________________________________________________________________________
#----                             getTimes                                  ----
#_______________________________________________________________________________

setMethod("getTimes", signature = c("dataset"), definition = function(object) {
  return(object@arms %>% getTimes())
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("dataset"), definition=function(x) {
  subjectsPerArm <- x@arms@list %>% purrr::map_int(.f=~.x@subjects) 
  return(sum(subjectsPerArm))
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
    data <- (covariate %>% sample(n=n))@distribution@sampled_values
    matrix <- matrix(data=data, ncol=1)
    colnames(matrix) <- covariate@name
    matrix %>% tibble::as_tibble()
  })
  return(retValue)
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

setMethod("export", signature=c("dataset", "character"), definition=function(object, dest, seed=NULL, event_related_column=FALSE, ...) {
  destinationEngine <- getSimulationEngineType(dest)
  table <- object %>% export(destinationEngine, seed=seed, ...)
  if (!event_related_column) {
    table <- table %>% dplyr::select(-EVENT_RELATED)
  }
  return(table)
})

setMethod("export", signature=c("dataset", "rxode_engine"), definition=function(object, dest, seed, ...) {

  args <- list(...)
  model <- args$model
  if (!is.null(model) && !is(model, "pmx_model")) {
    stop("Please provide a valid PMX model.")
  }
  
  # Set seed value
  setSeed(getSeed(seed))
  
  # Generate IIV only if model is provided
  if (is.null(model)) {
    iiv <- data.frame()
  } else {
    rxmod <- model %>% pmxmod::export(dest="RxODE")
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
      doseNumber <- (treatment@list[[treatment %>% length()]])@dose_number
    } else { 
      doseNumber <- 1 # Default
    }
    observations <- protocol@observations
    covariates <- arm@covariates
    treatmentIovs <- treatment@iovs

    # Generating subject ID's
    ids <- seq_len(subjects) + maxID - subjects

    # Create the base table with all treatment entries and observations
    table <- c(treatment@list, observations@list) %>% purrr::map_df(.f=~sample(.x, n=subjects, ids=ids, config=config, armID=armID))
    table <- table %>% dplyr::arrange(ID, TIME, EVID)
    
    # Sampling covariates
    cov <- sampleCovariatesList(covariates, n=length(ids))
    if (nrow(cov) > 0) {
      cov <- cov %>% tibble::add_column(ID=ids, .before=1)
      table <- table %>% dplyr::left_join(cov, by="ID")
    }
    
    # Treating IOV's
    iovsAsCov <- processAsCovariate(treatmentIovs@list)

    # Sampling IOV's
    iov <- sampleCovariatesList(iovsAsCov, n=length(ids)*doseNumber)
    if (nrow(iov) > 0) {
      iovInit <- data.frame(ID=rep(ids, each=doseNumber), DOSENO=rep(seq_len(doseNumber), length(ids)))
      iov <- cbind(iovInit, iov)
      table <- table %>% dplyr::left_join(iov, by=c("ID","DOSENO"))
    }
    
    # Joining IIV
    if (nrow(iiv) > 0) {
      table <- table %>% dplyr::left_join(iiv, by="ID")
    }
    return(table)
  })
  
  # Apply compartment properties coming from the model
  if (!is.null(model)) {
    retValue <- applyCompartmentCharacteristics(retValue, model@compartments@properties)
  }
  
  # Remove IS_INFUSION column
  retValue <- retValue %>% dplyr::select(-IS_INFUSION)
  
  # IOV post-processing
  
  # A few explanations: 
  # Before, order was: # 2 # 1 # 3
  # Now if 2 rows have the same time, IOV values will be carried backward
  # This is a 'workaround' for RxODE as it only reads the first covariate value for 2 rows belonging to the same time
  # I.e. most of the time OBS time X then DOSE time X, IOV value will be the IOV value of the observation
  iovNames <- object %>% getIOVNames()
  retValue <- retValue %>% dplyr::group_by(ID, TIME) %>% tidyr::fill(dplyr::all_of(iovNames), .direction="up")       # 1
  retValue <- retValue %>% dplyr::group_by(ID) %>% tidyr::fill(dplyr::all_of(iovNames), .direction="down")           # 2
  retValue <- retValue %>% dplyr::group_by(ID) %>% dplyr::mutate_at(.vars=iovNames, .funs=~ifelse(is.na(.x), 0, .x)) # 3

  return(retValue %>% dplyr::ungroup())
})

setMethod("export", signature=c("dataset", "mrgsolve_engine"), definition=function(object, dest, seed, ...) {
  
  # Same exported dataset as for RxODE
  return(object %>% export(dest=getSimulationEngineType("RxODE"), seed=seed, ...))
})
