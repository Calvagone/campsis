
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

setMethod("add", signature = c("dataset", "treatment_characteristic"), definition = function(object, x) {
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

setMethod("add", signature = c("dataset", "observation"), definition = function(object, x) {
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
#----                             length                                    ----
#_______________________________________________________________________________

setMethod("length", signature=c("dataset"), definition=function(x) {
  subjectsPerArm <- x@arms@list %>% purrr::map_int(.f=~.x@subjects) 
  return(sum(subjectsPerArm))
})

#_______________________________________________________________________________
#----                      hasModelDistribution                         ----
#_______________________________________________________________________________

setMethod("hasModelDistribution", signature = c("dataset"), definition = function(object) {
  res1 <- object@arms@list %>% purrr::map_lgl(~.x@protocol@treatment@characteristics %>% hasModelDistribution())
  res2 <- object@arms@list %>% purrr::map_lgl(~.x@protocol@treatment@iovs %>% hasModelDistribution())
  return(any(c(res1, res2)))
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
  iiv <- MASS::mvrnorm(n=n, mu=rep(0, nrow(omega)), Sigma=omega)
  if (n==1) {
    iiv <- t(iiv) # If n=1, mvrnorm result is a numeric vector, not a matrix
  }
  iiv <- iiv %>% as.data.frame()
  return(iiv)
}

#' Process characteristic IOV.
#' 
#' @param table current dataset, data frame form
#' @param characteristic characteristic being treated
#' @return updated table
#' 
processCharacteristicIOV <- function(table, characteristic) {
  distribution <- characteristic@distribution
  if (is(distribution, "parameter_distribution") && length(distribution@iov) > 0) {
    iov <- distribution@iov
    colName <- characteristic %>% getColumnName()
    if (!(iov %in% colnames(table))) {
      stop(paste0("There is no IOV column '", iov, "'. ", "Please add it to the dataset."))
    }
    # Only valid for log-normal distributions: exp(ETA1 + ETA2) = exp(ETA1)*exp(ETA2)
    charValue <- table[, colName]
    table[, colName] <- ifelse(is.na(charValue), charValue, charValue*exp(table[, iov]))
    return(table)
  } else {
    return(table)
  }
}

#' Process bioavailabilities.
#' 
#' @param table current dataset, data frame form
#' @param characteristics all treatment characteristics
#' @return updated table (AMT adapted)
#' 
processBioavailabilities <- function(table, characteristics) {
  bioavailabilities <- characteristics %>% pmxmod::select("bioavailability")
  if (bioavailabilities %>% length() == 0) {
    return(table)
  }
  colToRemove <- NULL
  for (bioavailability in bioavailabilities@list) {
    colName <- bioavailability %>% getColumnName()
    compartment <- bioavailability@compartment
    table <- processCharacteristicIOV(table, characteristic=bioavailability)
    table <- table %>% dplyr::mutate(
      AMT=ifelse(table$EVID==1 & table$CMT==compartment,
                  table$AMT * table[,colName],
                  table$AMT))
    colToRemove <- c(colToRemove, colName)
  }
  table <- table %>% dplyr::select(-dplyr::all_of(colToRemove))
  return(table)
}

#' Process infusions.
#' 
#' @param table current dataset, data frame form
#' @param characteristics all treatment characteristics
#' @return updated table with RATE column
#' 
processInfusions <- function(table, characteristics) {
  durations <- characteristics %>% pmxmod::select("infusion_duration")
  if (durations %>% length() == 0) {
    return(table)
  }
  table <- table %>% tibble::add_column(RATE=0, .after="AMT")
  colToRemove <- NULL
  for (duration in durations@list) {
    colName <- duration %>% getColumnName()
    compartment <- duration@compartment
    table <- processCharacteristicIOV(table, characteristic=duration)
    if (duration@rate) {
      table <- table %>% dplyr::mutate(
        RATE=ifelse(table$EVID==1 & table$CMT==compartment,
                    table[,colName],
                    table$RATE))
    } else {
      table <- table %>% dplyr::mutate(
        RATE=ifelse(table$EVID==1 & table$CMT==compartment,
                    table$AMT / table[,colName],
                    table$RATE))
    }
    colToRemove <- c(colToRemove, colName)
  }
  table <- table %>% dplyr::select(-dplyr::all_of(colToRemove))
  return(table)
}

#' Process lag times.
#' 
#' @param table current dataset, data frame form
#' @param characteristics all treatment characteristics
#' @return updated table with time of doses updated
#' 
processLagTimes <- function(table, characteristics) {
  lagTimes <- characteristics %>% pmxmod::select("lag_time")
  if (lagTimes %>% length() == 0) {
    return(table)
  }
  colToRemove <- NULL
  for (lagTime in lagTimes@list) {
    colName <- lagTime %>% getColumnName()
    compartment <- lagTime@compartment
    table <- processCharacteristicIOV(table, characteristic=lagTime)
    table <- table %>% dplyr::mutate(
      TIME=ifelse(table$EVID==1 & table$CMT==compartment,
                  table$TIME + table[,colName],
                  table$TIME))
    colToRemove <- c(colToRemove, colName)
  }
  table <- table %>% dplyr::select(-dplyr::all_of(colToRemove))
  table <- table %>% dplyr::arrange(ID, TIME)
  return(table)
}

#' Process distribution-derived objects as covariates.
#' 
#' @param list list of distribution-derived objects
#' @param iiv IIV pre-computed data frame
#' @param rxmod prepared data for RxODE
#' @param ids subject ID's being simulated in arm
#' @return a list of covariates that can be easily processed
#' 
processAsCovariate <- function(list, iiv, rxmod, ids) {
  covariates <- new("covariates")
  for (distribution in list) {
    dist <- distribution@distribution
    
    if (is(dist, "sampled_distribution")) {
      name <- distribution %>% getColumnName()
      covariates <- covariates %>% add(Covariate(name=name, distribution=dist))
      
    } else if(is(dist, "model_distribution")) {
      name <- distribution %>% getColumnName()
      theta <- dist@theta
      omega <- dist@omega
      thetaName <- paste0("THETA_", theta)
      etaName <- paste0("ETA_", omega)
      
      if (is(dist, "parameter_distribution")) {
        if (!hasName(rxmod@theta, thetaName)) {
          stop(paste0("'", thetaName, "'", " not part of the THETA vector"))
        }
        if (length(omega) > 0 && !hasName(iiv, etaName)) {
          stop(paste0("'", etaName, "'", " not part of the IIV matrix"))
        }
        mean <- rxmod@theta[[thetaName]]
        if (length(omega) > 0) {
          var <- iiv[ids, etaName]
          covariates <- covariates %>% add(Covariate(name=name, distribution=FixedDistribution(mean*exp(var))))
        } else {
          covariates <- covariates %>% add(Covariate(name=name, distribution=ConstantDistribution(mean)))
        }
      } else if (is(dist, "eta_distribution")) {
        if (!(etaName %in% colnames(rxmod@omega))) {
          stop(paste0("'", etaName, "'", " not part of the OMEGA matrix"))
        }
        var <- rxmod@omega[[etaName, etaName]]
        distribution <- FunctionDistribution(fun="rnorm", args=list(mean=0, sd=sqrt(var)))
        covariates <- covariates %>% add(Covariate(name=name, distribution=distribution))
      } else {
        stop(paste0("Unknown distribution class: ", as.character(class(dist))))
      }
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
#' 
#' @param table current dataset
#' @param characteristics compartment characteristics from model
#' @return updated dataset
#' @importFrom dplyr mutate
#' 
applyCompartmentCharacteristics <- function(table, characteristics) {
  for (charateristic in characteristics@list) {
    if (is(charateristic, "compartment_infusion_duration")) {
      compartment <- charateristic@compartment
      if (!("RATE" %in% colnames(table))) {
        table <- table %>% dplyr::mutate(RATE=0)
      }
      rateValue <- ifelse(charateristic@rate, -1, -2)
      table <- table %>% dplyr::mutate(RATE=ifelse(EVID==1 & CMT==compartment, rateValue, RATE))
    }
  }
  return(table)
}


setMethod("export", signature=c("dataset", "character"), definition=function(object, dest, ...) {
  if (dest=="RxODE") {
    return(object %>% export(new("rxode_engine"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("export", signature=c("dataset", "rxode_engine"), definition=function(object, dest, ...) {

  args <- list(...)
  model <- args$model
  if (!is.null(model) && !is(model, "pmx_model")) {
    stop("Please provide a valid PMX model.")
  }
  
  # Set seed value only if requested
  seed <- processExtraArg(args, name="seed")
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generate IIV only if model is provided
  if (is.null(model)) {
    iiv <- data.frame()
    if (object %>% hasModelDistribution()) {
      stop("Dataset contains at least one parameter-related distribution. Please provide a PMX model.")
    }
  } else {
    rxmod <- model %>% pmxmod::export(dest="RxODE")
    iiv <- generateIIV(omega=rxmod@omega, n=object %>% length())
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
    characteristics <- treatment@characteristics
    treatmentIovs <- treatment@iovs

    # Fill in entries list
    entries <- new("time_entries")
    entries@list <- c(entries@list, treatment@list)
    entries@list <- c(entries@list, observations@list)
    
    # Sort entries
    entries <- entries %>% pmxmod::sort()

    # Interesting part
    df <- entries@list %>% purrr::map_df(.f=~convert(.x, config)) %>% dplyr::select(-DV)

    # Generating subject ID's
    ids <- seq_len(subjects) + maxID - subjects

    # Treating treatment characteristics as covariates in the dataset
    characteristicsAsCov <- processAsCovariate(characteristics@list, iiv=iiv, rxmod=rxmod, ids=ids)
    covariates <- covariates %>% add(characteristicsAsCov)
    
    # Sampling covariates
    cov <- sampleCovariatesList(covariates, n=length(ids))
    
    # Treating IOV's (note that IIV is not needed)
    iovsAsCov <- processAsCovariate(treatmentIovs@list, iiv=data.frame(), rxmod=rxmod, ids=ids)

    # Sampling IOV's
    iov <- sampleCovariatesList(iovsAsCov, n=length(ids)*doseNumber)
    if (nrow(iov) > 0) {
      iovInit <- data.frame(ID=rep(ids, each=doseNumber), DOSENO=rep(seq_len(doseNumber), length(ids)))
      iov <- cbind(iovInit, iov)
    }

    # Expanding the dataframe for all subjects
    expDf <- ids %>% purrr::map_df(.f=function(id) {
      df <- df %>% tibble::add_column(ID=id, .before="TIME")
      df <- df %>% tibble::add_column(ARM=armID, .before="TIME")
      if (nrow(cov) > 0) {
        df <- df %>% dplyr::bind_cols(cov[(id - maxID + subjects),])
      }
      if (nrow(iiv) > 0) {
        df <- df %>% dplyr::bind_cols(iiv[id,])
      }
      return(df)
    })
    
    # Left join IOV, by subject ID and dose number
    if (nrow(iov) > 0) {
      expDf <- expDf %>% dplyr::left_join(iov, by = c("ID", "DOSENO"))
    }

    # Treating bioavailabilities
    expDf <- processBioavailabilities(expDf, characteristics)
    
    # Treating infusion durations
    expDf <- processInfusions(expDf, characteristics)
    
    # Treating lag times
    expDf <- processLagTimes(expDf, characteristics)
    
    return(expDf)
  })
  
  # Apply compartment characteristics coming from the model
  if (!is.null(model)) {
    retValue <- applyCompartmentCharacteristics(retValue, model@compartments@characteristics)
  }
  
  return(retValue)
})
