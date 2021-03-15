
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
#----                      hasParameterDistribution                         ----
#_______________________________________________________________________________

setMethod("hasParameterDistribution", signature = c("dataset"), definition = function(object) {
  return(any(object@arms@list %>% purrr::map_lgl(~.x@protocol@treatment@characteristics %>% hasParameterDistribution())))
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
    iivDf <- data.frame()
    if (object %>% hasParameterDistribution()) {
      stop("Dataset contains at least one parameter-related distribution. Please provide a PMX model.")
    }
  } else {
    rxmod <- model %>% pmxmod::export(dest="RxODE")
    iivDf <- generateIIV(omega=rxmod@omega, n=object %>% length())
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
    doseNumber <- (treatment@list[[treatment %>% length()]])@dose_number
    observations <- protocol@observations
    covariates <- arm@covariates
    characteristics <- treatment@characteristics
    iovs <- treatment@iovs

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
    for (characteristic in characteristics@list) {
      dist <- characteristic@distribution
      
      if (is(dist, "sampled_distribution")) {
        name <- characteristic %>% getColumnName()
        covariates <- covariates %>% add(Covariate(name=name, distribution=dist))
      
      } else if(is(dist, "parameter_distribution")) {
        name <- characteristic %>% getColumnName()
        thetaName <- characteristic@distribution@theta_name
        etaName <- characteristic@distribution@eta_name
        mean <- rxmod@theta[[paste0("THETA_", thetaName)]]
        var <- iivDf[ids, paste0("ETA_", etaName)]
        covariates <- covariates %>% add(Covariate(name=name, distribution=FixedDistribution(mean*exp(var))))
      
      } else {
        stop(paste0("Unknown distribution class: ", as.character(class(dist))))
      }
    }
    
    # Generating covariates
    covDf <- covariates@list %>% purrr::map_dfc(.f=function(covariate) {
      data <- (covariate %>% sample(n=length(ids)))@distribution@sampled_values
      matrix <- matrix(data=data, ncol=1)
      colnames(matrix) <- covariate@name
      matrix %>% tibble::as_tibble()
    })
    
    # Treating IOVs
    covariates <- new("covariates")
    for (iov in iovs@list) {
      dist <- iov@distribution
      
      if (is(dist, "sampled_distribution")) {
        name <- iov@colname
        covariates <- covariates %>% add(Covariate(name=name, distribution=dist))
        
      } else if(is(dist, "parameter_distribution")) {
        name <- iov %>% getColumnName()
        thetaName <- iov@distribution@theta_name
        etaName <- iov@distribution@eta_name
        mean <- rxmod@theta[[paste0("THETA_", thetaName)]]
        var <- iivDf[ids, paste0("ETA_", etaName)]
        covariates <- covariates %>% add(Covariate(name=name, distribution=FixedDistribution(mean*exp(var))))
        
      } else {
        stop(paste0("Unknown distribution class: ", as.character(class(dist))))
      }
    }
    
    # Generating IOVs
    iovDf <- covariates@list %>% purrr::map_df(.f=function(covariate) {
      retValue <- data.frame(ID=rep(ids, each=doseNumber), DOSENO=rep(seq_len(doseNumber), length(ids)))
      retValue[, covariate@name] <- (covariate %>% sample(n=length(ids)*doseNumber))@distribution@sampled_values
      return(retValue)
    })

    # Expanding the dataframe for all subjects
    expDf <- ids %>% purrr::map_df(.f=function(id) {
      df <- df %>% tibble::add_column(ID=id, .before="TIME")
      df <- df %>% tibble::add_column(ARM=armID, .before="TIME")
      if (nrow(covDf) > 0) {
        df <- df %>% dplyr::bind_cols(covDf[(id - maxID + subjects),])
      }
      if (nrow(iivDf) > 0) {
        df <- df %>% dplyr::bind_cols(iivDf[id,])
      }
      return(df)
    })
    
    # Left join IOV, by subject ID and dose number
    if (nrow(iovDf) > 0) {
      expDf <- expDf %>% dplyr::left_join(iovDf, by = c("ID", "DOSENO"))
    }

    # Treating bioavailabilities
    expDf <- processBioavailabilities(expDf, characteristics)
    
    # Treating infusion durations
    expDf <- processInfusions(expDf, characteristics)
    
    # Treating lag times
    expDf <- processLagTimes(expDf, characteristics)
    
    return(expDf)
  })
  
  return(retValue)
})