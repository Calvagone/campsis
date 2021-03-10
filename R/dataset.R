
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
#' @return a dataset
#' @export
Dataset <- function() {
  return(new("dataset"))
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
#----                           generate_iiv                                ----
#_______________________________________________________________________________

#' Generate IIV in dataset.
#' 
#' @param dataset dataset
#' @param model model
#' @return dataset
#' @export
generateIIV <- function(dataset, model) {
  stop("No default function is provided")
}

setGeneric("generateIIV", function(dataset, model) {
  standardGeneric("generateIIV")
})

setMethod("generateIIV", signature=c("dataset", "pmx_model"), definition=function(dataset, model) {
  rxmod <- model %>% pmxmod::export(dest="RxODE")
  omega <- rxmod@omega
  n <- dataset %>% length()
  iiv <- MASS::mvrnorm(n=n, mu=rep(0, nrow(omega)), Sigma=omega)
  if (n==1) {
    iiv <- t(iiv) # If n=1, mvrnorm result is a numeric vector, not a matrix
  }
  iiv <- iiv %>% as.data.frame()
  retValue <- dataset
  retValue@iiv <- iiv 
  
  return(retValue)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________


setMethod("export", signature=c("dataset", "character"), definition=function(object, dest, ...) {
  if (dest=="RxODE") {
    return(object %>% export(new("rxode_engine"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("export", signature=c("dataset", "rxode_engine"), definition=function(object, dest, ...) {

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
    
    # Treating lag time as a covariate
    if (lagTimes %>% length() > 0) {
      for (lagTime in lagTimes@list) {
        covariates <- covariates %>% add(FunctionCovariate(
          name=paste0("ALAG", lagTime@compartment),
          fun="rnorm", args=list(mean=lagTime@mean, sd=sqrt(lagTime@variance))))
      }
    }
    
    # Generating covariates
    covDf <- covariates@list %>% purrr::map_dfc(.f=function(covariate) {
      data <- (covariate %>% sample(n=length(ids)))@sampled_values
      matrix <- matrix(data=data, ncol=1)
      colnames(matrix) <- covariate@name
      matrix %>% tibble::as_tibble()
    })
    
    # IIV df
    iivDf <- object@iiv
    
    # Expanding the dataframe for all subjects
    expandedDf <- ids %>% purrr::map_df(.f=function(id) {
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
    
    # Treating lag time
    if (lagTimes %>% length() > 0) {
      colToRemove <- NULL
      for (lagTime in lagTimes@list) {
        colName <- paste0("ALAG", lagTime@compartment)
        compartment <- lagTime@compartment
        expandedDf <- expandedDf %>% dplyr::mutate(
          TIME=ifelse(expandedDf$EVID==1 & expandedDf$CMT==compartment,
                      expandedDf$TIME + expandedDf[,colName],
                      expandedDf$TIME))
        colToRemove <- c(colToRemove, colName)
      }
      expandedDf <- expandedDf %>% dplyr::select(-dplyr::all_of(colToRemove))
      expandedDf <- expandedDf %>% dplyr::group_by(ID) %>% dplyr::arrange(TIME) %>% dplyr::ungroup()
    }
    
    return(expandedDf)
  })
  
  return(retValue)
})