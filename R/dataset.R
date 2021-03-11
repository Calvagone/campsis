
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

setMethod("add", signature = c("dataset", "lag_time"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@treatment <- arm@protocol@treatment %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
})

setMethod("add", signature = c("dataset", "infusion_duration"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@treatment <- arm@protocol@treatment %>% add(x)
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
  if (is.null(model) || !is(model, "pmx_model")) {
    stop("Please provide a model to export the dataset.")
  }
  
  # Need RxODE model to access THETA's and OMEGA's
  rxmod <- model %>% pmxmod::export(dest="RxODE")
  omega <- rxmod@omega
  theta <- rxmod@theta
  
  # Generate IIV
  iivDf <- generateIIV(omega=omega, n=object %>% length())
  
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
    observations <- protocol@observations
    covariates <- arm@covariates
    lagTimes <- treatment@lag_times
    infusionDurations <- treatment@infusion_durations
    
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

    # Treating infusion duration & lag times as a covariate
    for (specialVariable in c(lagTimes@list, infusionDurations@list)) {
      dist <- specialVariable@distribution
      
      if (is(dist, "sampled_distribution")) {
        name <- specialVariable %>% getColumnName()
        covariates <- covariates %>% add(Covariate(name=name, distribution=dist))
      
      } else if(is(dist, "parameter_distribution")) {
        name <- specialVariable %>% getColumnName()
        thetaName <- specialVariable@distribution@theta_name
        etaName <- specialVariable@distribution@eta_name
        mean <- theta[[paste0("THETA_", thetaName)]]
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
    
    # Treating infusion durations
    if (infusionDurations %>% length() > 0) {
      expDf <- expDf %>% tibble::add_column(RATE=0, .after="AMT")
      colToRemove <- NULL
      for (duration in infusionDurations@list) {
        colName <- duration %>% getColumnName()
        compartment <- duration@compartment
        if (duration@rate) {
          expDf <- expDf %>% dplyr::mutate(
            RATE=ifelse(expDf$EVID==1 & expDf$CMT==compartment,
                        expDf[,colName],
                        expDf$RATE))
        } else {
          expDf <- expDf %>% dplyr::mutate(
            RATE=ifelse(expDf$EVID==1 & expDf$CMT==compartment,
                        expDf$AMT / expDf[,colName],
                        expDf$RATE))
        }
        colToRemove <- c(colToRemove, colName)
      }
      expDf <- expDf %>% dplyr::select(-dplyr::all_of(colToRemove))
    }
    
    # Treating lag times
    if (lagTimes %>% length() > 0) {
      colToRemove <- NULL
      for (lagTime in lagTimes@list) {
        colName <- lagTime %>% getColumnName()
        compartment <- lagTime@compartment
        expDf <- expDf %>% dplyr::mutate(
          TIME=ifelse(expDf$EVID==1 & expDf$CMT==compartment,
                      expDf$TIME + expDf[,colName],
                      expDf$TIME))
        colToRemove <- c(colToRemove, colName)
      }
      expDf <- expDf %>% dplyr::select(-dplyr::all_of(colToRemove))
      expDf <- expDf %>% dplyr::group_by(ID) %>% dplyr::arrange(TIME) %>% dplyr::ungroup()
    }
    
    return(expDf)
  })
  
  return(retValue)
})