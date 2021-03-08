
#_______________________________________________________________________________
#----                         dataset class                                 ----
#_______________________________________________________________________________

#' @export
setClass(
  "dataset",
  representation(
    arms = "arms"
  ),
  prototype=prototype(arms=new("arms"))
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
}
)

setMethod("add", signature = c("dataset", "treatment_entry"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@treatment <- arm@protocol@treatment %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
  }
)

setMethod("add", signature = c("dataset", "observation"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@protocol@observations <- arm@protocol@observations %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
}
)

setMethod("add", signature = c("dataset", "covariate"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  arm <- object@arms %>% default()
  arm@covariates <- arm@covariates %>% add(x)
  object@arms <- object@arms %>% pmxmod::replace(arm)
  return(object)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________


setMethod("export", signature=c("dataset", "character"), definition=function(object, dest, ...) {
  if (dest=="RxODE") {
    return(object %>% export(new("rxode_type"), ...))
  } else {
    stop("Only RxODE is supported for now")
  }
})

setMethod("export", signature=c("dataset", "rxode_type"), definition=function(object, dest, ...) {
  # Check extra arguments
  args <- list(...)

  # Retrieve the config argument if present or create a new one
  if (hasName(args, "config")) {
    config <- args$config
  } else {
    config <- new("dataset_config")
  }
  
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
    
    # Fill in entries list
    entries <- new("time_entries")
    treatment@list %>% purrr::map(.f=function(entry) {
      entries <<- entries %>% add(entry)
    })
    observations@list %>% purrr::map(.f=function(entry) {
      entries <<- entries %>% add(entry)
    })
    
    # Sort entries
    entries <- entries %>% pmxmod::sort()

    # Interesting part
    df <- entries@list %>% purrr::map_df(.f=~convert(.x, config))

    # Generating subject ID's
    ids <- seq_len(subjects) + maxID - subjects
    
    # Generating covariates
    covDf <- covariates@list %>% purrr::map_dfc(.f=function(covariate) {
      data <- (covariate %>% sample(n=length(ids)))@sampled_values
      matrix <- matrix(data=data, ncol=1)
      colnames(matrix) <- covariate@name
      matrix %>% tibble::as_tibble()
    })
    
    # Expanding the dataframe for all subjects
    expandedDf <- ids %>% purrr::map_df(.f=function(id) {
      df <- df %>% tibble::add_column(ID=id, .before="TIME")
      df <- df %>% tibble::add_column(ARM=armID, .before="TIME")
      if (nrow(covDf) > 0) {
        df <- df %>% dplyr::bind_cols(covDf[(id - maxID + subjects),])
      }
      return(df)
    })
    
    return(expandedDf)
  })
  
  return(retValue)
})