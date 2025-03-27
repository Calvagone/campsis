
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
    arms="arms",
    config="dataset_config",
    iiv="data.frame"
  ),
  prototype=prototype(arms=new("arms"), config=DatasetConfig(), iiv=data.frame())
)

#'
#' Create a dataset.
#'
#' @param subjects number of subjects in the default arm
#' @param label label of the default arm, NA by default
#' @return a dataset
#' @export
Dataset <- function(subjects=NULL, label=as.character(NA)) {
  arms=new("arms")
  if (!is.null(subjects)) {
    arm <- arms %>% default()
    arm@subjects <- as.integer(subjects)
    arm@label <- as.character(label)
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
  for (armIndex in seq_along(object@arms@list)) {
    object@arms@list[[armIndex]] <- object@arms@list[[armIndex]] %>% add(x)
  }
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

setMethod("find", signature = c("dataset", "arm"), definition = function(object, x) {
  return(object@arms %>% find(x))
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
#' @importFrom methods validObject
setMethod("setSubjects", signature = c("dataset", "integer"), definition = function(object, x) {
  object <- object %>% createDefaultArmIfNotExists()
  numberOfArms <- object@arms %>% length()
  assertthat::assert_that(length(x)==numberOfArms, msg="x must be the same length as the number of arms in dataset")
  for (armIndex in seq_len(numberOfArms)) {
    arm <- object@arms@list[[armIndex]]
    arm <- arm %>% setSubjects(x[armIndex])
    object <- object %>% replace(arm)
  }
  methods::validObject(object)
  return(object)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Generate IIV matrix for the given OMEGA matrix.
#' 
#' @param omega omega matrix
#' @param n number of subjects
#' @return IIV data frame
#' @export
generateIIV_ <- function(omega, n) {
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

#' Generate IIV matrix for the given Campsis model.
#' 
#' @param model Campsis model
#' @param n number of subjects
#' @param offset if specified, resulting ID will be ID + offset
#' @return IIV data frame with ID column
#' @export
generateIIV <- function(model, n, offset=0) {
  # Generate IIV only if model is provided
  if (is.null(model)) {
    iiv <- data.frame()
  } else {
    rxmod <- model %>% export(dest="RxODE")
    iiv <- generateIIV_(omega=rxmod@omega, n=n)
    if (nrow(iiv) > 0) {
      iiv <- iiv %>% tibble::add_column(ID=seq_len(n) + offset, .before=1)
    }
  }
  return(iiv)
}

#' Left-join IIV matrix.
#' 
#' @param table dataset, tabular form
#' @param iiv IIV matrix
#' @return updated table with IIV matrix
#' @keywords internal
leftJoinIIV <- function(table, iiv) {
  if (nrow(iiv) > 0) {
    table <- table %>% dplyr::left_join(iiv, by="ID")
  }
  return(table)
}

#' Sample covariates list.
#' 
#' @param covariates list of covariates to sample
#' @param ids_within_arm ids within the current arm being sampled
#' @param subset take subset of original values because export is parallelised
#' @return a dataframe of n rows, 1 column per covariate
#' @keywords internal
#' 
sampleCovariatesList <- function(covariates, ids_within_arm, subset) {
  n <- length(ids_within_arm)
  retValue <- covariates@list %>% purrr::map_dfc(.f=function(covariate) {
    distribution <- covariate@distribution
    if (subset && is(distribution, "fixed_distribution")) {
      distribution@values <- distribution@values[ids_within_arm]
      # print(ids_within_arm)
      assertthat::assert_that(!any(is.na(distribution@values)),
                              msg=paste0("NA's detected in covariate '", covariate@name, "'"))
    }
    sampleDistributionAsTibble(distribution, n=n, colname=covariate@name)
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
      table <- table %>% dplyr::mutate(
        RATE=ifelse(.data$EVID==1 & .data$CMT==compartment & .data$INFUSION_TYPE==0,
                    rateValue, .data$RATE))
    }
  }
  return(table)
}

#' @importFrom dplyr all_of
setMethod("export", signature=c("dataset", "character"), definition=function(object, dest, seed=NULL, model=NULL, settings=NULL, event_related_column=FALSE) {
  destinationEngine <- getSimulationEngineType(dest)
  settings <- preprocessSettings(settings, dest) # In case of NULL settings
  table <- object %>% export(dest=destinationEngine, seed=seed, model=model, settings=settings)
  if (!event_related_column) {
    table <- table %>% dplyr::select(-dplyr::all_of("EVENT_RELATED"))
  }
  return(table)
})

#' Get a mapping table with all possibilities of compartment names and their indexes
#' knowing that compartment names can be provided as character or as integer.
#' 
#' @param compartments list of compartments
#' @return a tibble with two columns: INDEX and NAME
#' @keywords internal
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom assertthat assert_that
#' 
getCompartmentMapping <- function(compartments) {
  if (length(compartments)==0) {
    return(tibble::tibble(INDEX=as.integer(0), NAME=as.character(0)))
  }
  compartmentMapping <- compartments@list %>%
    purrr::map_dfr(.f=function(cmt) {
      if (is.na(cmt@name)) {
        return(tibble::tibble(INDEX=cmt@index, NAME=as.character(cmt@index)))
      } else {
        assertthat::assert_that(!(nchar(cmt@name)==1 && cmt@name != as.character(cmt@index)),
                                msg=paste0("Compartment name '%s' not corresponding to its index", cmt@name))
        return(tibble::tibble(INDEX=c(cmt@index, cmt@index), NAME=c(cmt@name, as.character(cmt@index))))
      }
    })
  return(compartmentMapping)
}

#' Export delegate method. This method is common to RxODE and mrgsolve.
#' 
#' @param object current dataset
#' @param dest destination engine
#' @param model Campsis model, if provided, ETA's will be added to the dataset
#' @param arm_offset arm offset (on ID's) to apply when parallelisation is used.
#' Default value is NULL, meaning parallelisation is disabled. Otherwise, it corresponds
#' to the offset to apply for the current arm being exported (in parallel).
#' @param offset_within_arm offset (on ID's) to apply within the current arm being
#'  exported (only used when parallelisation is enabled), default is 0
#' @return 2-dimensional dataset, same for RxODE and mrgsolve
#' @importFrom dplyr across all_of arrange bind_rows group_by left_join recode
#' @importFrom campsismod export
#' @importFrom tibble add_column tibble
#' @importFrom purrr accumulate map_df map_int map2_df
#' @importFrom rlang parse_expr
#' @keywords internal
#' 
exportDelegate <- function(object, dest, model, arm_offset=NULL, offset_within_arm=0) {

  # Retrieve dataset configuration
  config <- object@config

  # Subset covariates if parallelisation is enabled (arm_offset != NULL)
  subsetCovariates <- !is.null(arm_offset)
  
  # Use either arms or default_arm
  arms <- object@arms
  if (length(arms) == 0) {
    stop("No entry in dataset. Not able to export anything...")
  }
  
  # Compute max ID per arm
  maxIDPerArm <- arms@list %>% purrr::map_int(~.x@subjects) %>% purrr::accumulate(~(.x+.y))
 
  # Get compartment mapping
  compartmentMapping <- NULL
  if (!is.null(model)) {
    compartmentMapping <- getCompartmentMapping(model@compartments)
  }
  
  retValue <- purrr::map2_df(arms@list, maxIDPerArm, .f=function(arm, maxID) {
    armID <- arm@id
    subjects <- arm@subjects
    protocol <- arm@protocol
    bootstrap <- arm@bootstrap
    
    # Unwrap treatment and assign dose number
    treatment <- protocol@treatment %>%
      unwrapTreatment() %>%
      assignDoseNumber()
    
    if (treatment %>% length() > 0) {
      maxDoseNumber <- (treatment@list[[treatment %>% length()]])@dose_number
    } else { 
      maxDoseNumber <- 1 # Default
    }
    observations <- protocol@observations
    # covariates = initial covariates + covariates from bootstrap
    covariates <- arm@covariates %>% add(bootstrap %>% sample(subjects))
    timeVaryingCovariates <- covariates %>% campsismod::select("time_varying_covariate")
    treatmentIovs <- treatment@iovs
    occasions <- treatment@occasions
    doseAdaptations <- treatment@dose_adaptations
    
    # Generating subject ID's
    if (is.null(arm_offset)) {
      arm_offset <- maxID - subjects
    }
    ids_within_arm <- seq_len(subjects) + offset_within_arm # ID's within arm
    ids <- ids_within_arm + arm_offset                      # ID's within dataset
    
    # Create the base table with all treatment entries and observations
    needsDV <- observations@list %>% purrr::map_lgl(~.x@dv %>% length() > 0) %>% any()
    table <- c(treatment@list, observations@list) %>%
      purrr::map_df(.f=~sample(.x, n=subjects, ids=ids, config=config, armID=armID, needsDV=needsDV))
    table <- table %>% dplyr::arrange(dplyr::across(c("ID","TIME","EVID")))

    # Sampling covariates
    cov <- sampleCovariatesList(covariates, ids_within_arm=ids_within_arm, subset=subsetCovariates)
    
    if (nrow(cov) > 0) {
      # Retrieve all covariate names (including time-varying ones)
      allCovariateNames <- colnames(cov)
      
      # Left join all covariates as fixed (one value per subjet)
      cov <- cov %>% tibble::add_column(ID=ids, .before=1)
      table <- table %>% dplyr::left_join(cov, by="ID")

      # Retrieve time-varying covariate names
      timeVaryingCovariateNames <- timeVaryingCovariates %>% getNames()
      
      # Merge time-varying covariate names
      if (timeVaryingCovariateNames %>% length() > 0) {
        # Only keep first row. Please note that NA's will be filled in 
        # by the final export method (depending on variables nocb & nocbvars)
        table <- table %>% dplyr::group_by(dplyr::across("ID")) %>%
          dplyr::mutate_at(.vars=timeVaryingCovariateNames,
                           .funs=~ifelse(dplyr::row_number()==1, .x, as.numeric(NA))) %>%
          dplyr::ungroup()
        
        # Merge all time varying covariate tables into a single table
        # The idea is to use 1 EVID=2 row per subject time
        timeCov <- mergeTimeVaryingCovariates(covariates=timeVaryingCovariates,
                                              ids_within_arm=ids_within_arm, arm_offset=arm_offset) %>%
          sampleTimeVaryingCovariates(armID=armID, needsDV=needsDV)

        # Bind with treatment and observations and sort
        table <- dplyr::bind_rows(table, timeCov)
        table <- table %>% dplyr::arrange(dplyr::across(c("ID","TIME","EVID")))
        
        # Fill NA values of fixed covariates that were introduced by EVID=2 rows
        table <- table %>% dplyr::group_by(dplyr::across("ID")) %>%
          tidyr::fill(allCovariateNames[!(allCovariateNames %in% timeVaryingCovariateNames)], .direction="down") %>%
          dplyr::ungroup()
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
    
    # Joining occasions
    for (occasion in occasions@list) {
      occ <- tibble::tibble(DOSENO=occasion@dose_numbers, !!occasion@colname:=occasion@values)
      table <- table %>% dplyr::left_join(occ, by="DOSENO")
    }
    
    # Recode compartments names according to their indexes
    table <- table %>%
      mutate(CMT=recodeCompartments(.data$CMT, compartmentMapping))

    # Apply formula if dose adaptations are present
    for (doseAdaptation in doseAdaptations@list) {
      compartments <- recodeCompartments(doseAdaptation@compartments, compartmentMapping)
      expr <- rlang::parse_expr(doseAdaptation@formula)
      # If a duration was specified, same duration applies on new AMT (i.e. RATE is recomputed)
      # If a rate was specified, same rate applies on new AMT (nothing to do)
      if (compartments %>% length() > 0) {
        table <- table %>% 
          dplyr::mutate(AMT_=ifelse(.data$CMT %in% compartments,
                                    eval(expr),
                                    .data$AMT),
                        RATE=ifelse((.data$CMT %in% compartments) & !is.na(.data$INFUSION_TYPE) & .data$INFUSION_TYPE==-2,
                                    .data$RATE*.data$AMT_/.data$AMT,
                                    .data$RATE))
      } else {
        table <- table %>% 
          dplyr::mutate(AMT_=eval(expr),
                        RATE=ifelse(!is.na(.data$INFUSION_TYPE) & .data$INFUSION_TYPE==-2,
                                    .data$RATE*.data$AMT_/.data$AMT,
                                    .data$RATE))
      }
      # Keep final rate and remove temporary column AMT_
      table <- table %>% dplyr::mutate(AMT=.data$AMT_) %>% dplyr::select(-dplyr::all_of("AMT_"))
    }
    
    return(table)
  })
  
  # Apply compartment properties coming from the model
  if (!is.null(model)) {
    retValue <- applyCompartmentCharacteristics(retValue, model@compartments@properties)
  }
  
  # Remove INFUSION_TYPE column
  retValue <- retValue %>% dplyr::select(-dplyr::all_of("INFUSION_TYPE"))
  
  # If TSLD or TDOS column is asked, we add TDOS column
  if (config@export_tsld || config@export_tdos) {
    retValue <- retValue %>% dplyr::mutate(TDOS=ifelse(.data$EVID==1, .data$TIME, NA))
  }
  
  return(retValue)
}

recodeCompartments <- function(x, compartmentMapping) {
  if (is.null(compartmentMapping)) {
    return(x)
  }
  cmtValues <- unique(x) %>% na.omit() # Character, but can be compartment indexes or names (or mixed)
  assert_that(all(cmtValues %in% compartmentMapping$NAME),
              msg=sprintf("Compartment name(s) %s are not found in the model",
                          paste0(compartmentMapping$NAME[!compartmentMapping$NAME %in% cmtValues], collapse="/")))
  # Argument .missing means that missing values in .x (NAs) are replaced by the provided value (NA)
  # Argument .default not set meaning we get a message if a compartment name is not found in the model
  return(as.integer(dplyr::recode(x, !!!setNames(compartmentMapping$INDEX, compartmentMapping$NAME))))
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
#' @importFrom dplyr across all_of group_by mutate_at
#' @importFrom tidyr fill
#' @keywords internal
#' 
fillIOVOccColumns <- function(table, columnNames, downDirectionFirst) {
  if (downDirectionFirst) {
    table <- table %>% dplyr::group_by(dplyr::across("ID")) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="down")           # 1
    table <- table %>% dplyr::group_by(dplyr::across(c("ID","TIME"))) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="up")      # 2
    table <- table %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::mutate_at(.vars=columnNames, .funs=~ifelse(is.na(.x), 0, .x)) # 3
  } else {
    table <- table %>% dplyr::group_by(dplyr::across(c("ID","TIME"))) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="up")      # 2
    table <- table %>% dplyr::group_by(dplyr::across("ID")) %>% tidyr::fill(dplyr::all_of(columnNames), .direction="down")           # 1
    table <- table %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::mutate_at(.vars=columnNames, .funs=~ifelse(is.na(.x), 0, .x)) # 3
  }
  return(table)
}

#' Counter-balance NOCB mode for occasions & IOV.
#' This function will simply shift all the related occasion & IOV columns to the right (by one).
#' 
#' @param table current table
#' @param columnNames columns to be counter-balanced
#' @return 2-dimensional dataset
#' @importFrom dplyr across group_by mutate_at n
#' @keywords internal
#'
counterBalanceNocbMode <- function(table, columnNames) {
  return(table %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::mutate_at(.vars=columnNames, .funs=~c(.x[1], .x[-dplyr::n()])))
}

#' Counter-balance LOCF mode for occasions & IOV.
#' This function will simply shift all the related occasion & IOV columns to the left (by one).
#' 
#' @param table current table
#' @param columnNames columns to be counter-balanced
#' @return 2-dimensional dataset
#' @importFrom dplyr across group_by mutate_at n
#' @keywords internal
#'
counterBalanceLocfMode <- function(table, columnNames) {
  return(table %>% dplyr::group_by(dplyr::across("ID")) %>% dplyr::mutate_at(.vars=columnNames, .funs=~c(.x[-1], .x[dplyr::n()])))
}

#' Get all time-varying variables. These variables are likely to be influenced
#' by the NOCB mode chosen and by the 'nocbvars' vector.
#' 
#' @param object dataset
#' @return character vector with all time-varying variables of the dataset
#' @keywords internal
#'
getTimeVaryingVariables <- function(object) {
  config <- object@config
  retValue <- c(object %>% getIOVs() %>% getNames(),
                object %>% getOccasions() %>% getNames(),
                object %>% getTimeVaryingCovariates() %>% getNames())
  if (config@export_tsld || config@export_tdos) {
    retValue <- retValue %>% append("TDOS")
  }
  if (config@export_tsld) {
    retValue <- retValue %>% append("TIME_TSLD")
  }
  return(retValue)
}

#' Preprocess TSLD and TDOS columns according to given dataset configuration.
#' 
#' @param table current table
#' @param config dataset config
#' @return updated table
#' @keywords internal
#'
preprocessTSLDAndTDOSColumn <- function(table, config) {
  if (config@export_tsld) {
    # Time column needs to be duplicated for the computation of TSLD
    # This is because TSLD is derived from TDOS and TIME_TSLD, and is 
    # sensitive to 'nocb'.
    table <- table %>% dplyr::mutate(TIME_TSLD=.data$TIME) # Duplicate TIME column
  }
  return(table)
}

#' Preprocess 'nocbvars' argument.
#' 
#' @param nocbvars nocbvars argument, character vector
#' @keywords internal
#' 
preprocessNocbvars <- function(nocbvars) {
  if ("TSLD" %in% nocbvars) {
    stop("As 'TSLD' is derived from 'TDOS', please use 'TDOS' in argument nocbvars")
  }
  # If 'TDOS' column is shifted because of 'nocb', then 'TIME_TSLD' also
  # must be shifted. This allows to correctly derive TSLD.
  if ("TDOS" %in% nocbvars) {
    nocbvars <- nocbvars %>% append("TIME_TSLD")
  }
  return(nocbvars)
}

#' Process time-related columns according to given dataset configuration.
#' 
#' @param table current table
#' @param config dataset config
#' @return updated table
#' @keywords internal
#'
processAllTimeColumns <- function(table, config) {
  unitFrom <- config@time_unit_dataset
  unitTo <- config@time_unit_export
  
  # TIME conversion according to specified units
  table <- table %>%
    dplyr::mutate(TIME=convertTime(x=.data$TIME, from=unitFrom, to=unitTo))
  
  # TDOS conversion according to specified units
  if (config@export_tdos || config@export_tsld) {
    table <- table %>%
      dplyr::mutate(TDOS=convertTime(x=.data$TDOS, from=unitFrom, to=unitTo))
  }
  
  # TIME_TSLD conversion according to specified units
  if (config@export_tsld) {
    table <- table %>%
      dplyr::mutate(TIME_TSLD=convertTime(x=.data$TIME_TSLD, from=unitFrom, to=unitTo))
  }
  
  # Compute TSLD
  if (config@export_tsld) {
    table <- table %>% dplyr::mutate(TSLD=.data$TIME_TSLD - .data$TDOS)
    table <- table %>% dplyr::select(-dplyr::all_of("TIME_TSLD"))
  }
  
  # Discard TDOS is not needed
  if (!config@export_tdos && config@export_tsld) {
    table <- table %>% dplyr::select(-dplyr::all_of("TDOS"))
  }
  return(table)
}


#' Get splitting configuration for parallel export.
#' 
#' @param dataset Campsis dataset to export
#' @param hardware hardware configuration
#' @return splitting configuration list (if 'parallel_dataset' is enabled) or
#'  NA (if 'parallel_dataset' disabled or if the length of the dataset is less than the dataset export slice size)
#' @export
#'
getSplittingConfiguration <- function(dataset, hardware) {
  
  sliceSize <- hardware@dataset_slice_size
  
  # Return NA if parallel export not needed
  if (!hardware@dataset_parallel) {
    return(NA)
  }
  
  # Splitting not needed if number of subject <= sliceSize 
  if (length(dataset) <= sliceSize) {
    return(NA)
  }
  
  # Split each arm according to the given dataset slice (size)
  retValue <- dataset@arms@list %>% purrr::imap(.f=function(arm, index) {
    subjects <- arm@subjects
    div <- subjects %/% sliceSize
    modulo <- subjects %% sliceSize
    subjects_ <- rep(sliceSize, div)
    if (modulo > 0) {
      subjects_ <- c(subjects_, modulo)
    }
    offset <- subjects_ %>%
      purrr::accumulate(~(.x+.y))
    offset <- c(0, offset[-length(offset)])
    return(list(subjects=subjects_, arm_index=index, offset_within_arm=offset))
  }) %>% purrr::map_dfr(.f=~tibble::tibble(subjects=as.integer(.x$subjects),
                                           arm_index=.x$arm_index,
                                           offset_within_arm=.x$offset_within_arm))
  
  # Left join arm offset
  armOffset <-  dataset@arms@list %>%
    purrr::map_int(~.x@subjects) %>%
    purrr::accumulate(~(.x + .y))
  armOffset <- c(0, armOffset[-length(armOffset)])
  retValue <- retValue %>%
    dplyr::left_join(tibble::tibble(arm_index=seq_along(dataset@arms@list), arm_offset=armOffset), by="arm_index")
  
  # Data frame to list conversion
  retValue <- split(retValue, seq(nrow(retValue)))
  
  return(retValue)
}


#' Split dataset according to config.
#' 
#' @param dataset Campsis dataset to export
#' @param config current iteration in future_map_dfr
#' @return a subset of the given dataset
#' @keywords internal
#'
splitDataset <- function(dataset, config) {
  if (is.list(config)) {
    arm <- dataset@arms@list[[config$arm_index]] %>% setSubjects(config$subjects)
    dataset@arms@list <- list(arm) # Only put previous arm into dataset
  }
  return(dataset)
}

setMethod("export", signature=c("dataset", "rxode_engine"), definition=function(object, dest, seed, model, settings) {
  
  # NOCB management
  nocb <- settings@nocb@enable
  nocbvars <- preprocessNocbvars(settings@nocb@variables)
  
  # Set seed value
  setSeed(getSeed(seed))
  
  # Generate IIV
  iiv <- generateIIV(model=model, n=length(object))
  
  # Retrieve splitting configuration
  configList <- getSplittingConfiguration(dataset=object, hardware=settings@hardware)
  furrrSeed <- if (is.list(configList)) {TRUE} else {NULL}
  
  retValue <- furrr::future_map_dfr(.x=configList, .f=function(config) {

    # Export table
    arm_offset <- if (is.list(config)) {config$arm_offset} else {NULL}
    offset_within_arm <- if (is.list(config)) {config$offset_within_arm} else {0}
    table <- exportDelegate(object=splitDataset(object, config), dest=dest, model=model,
                            arm_offset=arm_offset, offset_within_arm=offset_within_arm)

    # TSLD/TDOS pre-processing
    table <- table %>% preprocessTSLDAndTDOSColumn(config=object@config)
    
    # IOV / Occasion / Time-varying covariates post-processing
    iovOccNames <- getTimeVaryingVariables(object)
    iovOccNamesNocb <- iovOccNames[iovOccNames %in% nocbvars]
    iovOccNamesLocf <- iovOccNames[!(iovOccNames %in% nocbvars)]
    
    if (nocb) {
      table <- fillIOVOccColumns(table, columnNames=iovOccNamesNocb, downDirectionFirst=TRUE)
      table <- fillIOVOccColumns(table, columnNames=iovOccNamesLocf, downDirectionFirst=FALSE)
      table <- counterBalanceNocbMode(table, columnNames=iovOccNamesNocb)
    } else {
      table <- fillIOVOccColumns(table, columnNames=iovOccNames, downDirectionFirst=FALSE)
    }
    
    # Time-related columns processing
    table <- table %>% processAllTimeColumns(config=object@config)
    
    return(table %>% dplyr::ungroup())
  }, .options=furrr::furrr_options(seed=furrrSeed, scheduling=getFurrrScheduling(settings@hardware@dataset_parallel)))
  
  # Left-join IIV matrix
  retValue <- leftJoinIIV(table=retValue, iiv=iiv)
  
  return(retValue)
})

setMethod("export", signature=c("dataset", "mrgsolve_engine"), definition=function(object, dest, seed, model, settings) {
  
  # NOCB management
  nocb <- settings@nocb@enable
  nocbvars <- preprocessNocbvars(settings@nocb@variables)
  
  # Set seed value
  setSeed(getSeed(seed))
  
  # Generate IIV
  iiv <- generateIIV(model=model, n=length(object))

  # Retrieve splitting configuration
  configList <- getSplittingConfiguration(dataset=object, hardware=settings@hardware)
  furrrSeed <- if (is.list(configList)) {TRUE} else {NULL}

  retValue <- furrr::future_map_dfr(.x=configList, .f=function(config) {
    
    # Export table
    arm_offset <- if (is.list(config)) {config$arm_offset} else {NULL}
    offset_within_arm <- if (is.list(config)) {config$offset_within_arm} else {0}
    table <- exportDelegate(object=splitDataset(object, config), dest=dest, model=model,
                            arm_offset=arm_offset, offset_within_arm=offset_within_arm)
    
    # TSLD/TDOS pre-processing
    table <- table %>% preprocessTSLDAndTDOSColumn(config=object@config)
    
    # IOV / Occasion / Time-varying covariates post-processing
    iovOccNames <- getTimeVaryingVariables(object)
    iovOccNamesNocb <- iovOccNames[iovOccNames %in% nocbvars]
    iovOccNamesLocf <- iovOccNames[!(iovOccNames %in% nocbvars)]
    
    if (nocb) {
      table <- fillIOVOccColumns(table, columnNames=iovOccNames, downDirectionFirst=FALSE) # TRUE/FALSE not important (like NONMEM)
      table <- counterBalanceNocbMode(table, columnNames=iovOccNamesNocb)
    } else {
      table <- fillIOVOccColumns(table, columnNames=iovOccNames, downDirectionFirst=FALSE)
      table <- counterBalanceLocfMode(table, columnNames=iovOccNamesLocf)
    }
    
    # Time-related columns processing
    table <- table %>% processAllTimeColumns(config=object@config)
    
    return(table %>% dplyr::ungroup())
  }, .options=furrr::furrr_options(seed=furrrSeed, scheduling=getFurrrScheduling(settings@hardware@dataset_parallel)))
  
  # Left-join IIV matrix
  retValue <- leftJoinIIV(table=retValue, iiv=iiv)

  return(retValue)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("dataset"), definition=function(object) {
  if (object@arms@list %>% length() <= 1) {
    cat(paste0("Dataset (N=", object %>% length(), ")"))
    cat("\n")
  }
  show(object@arms)
})

#_______________________________________________________________________________
#----                          unwrapTreatment                              ----
#_______________________________________________________________________________

#' @rdname unwrapTreatment
setMethod("unwrapTreatment", signature=c("dataset"), definition = function(object) {
  object@arms <- object@arms %>% unwrapTreatment()
  return(object)
})
