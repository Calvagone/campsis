
#_______________________________________________________________________________
#----                        covariate class                                ----
#_______________________________________________________________________________

checkCovariate <- function(object) {
  return(expectOneForAll(object, c("name", "distribution")))
}

#' 
#' Covariate class.
#' 
#' @slot name covariate name, single character value
#' @slot distribution covariate distribution
#' @export
setClass(
  "covariate",
  representation(
    name = "character",
    distribution = "distribution"
  ),
  contains="pmx_element",
  validity=checkCovariate
)

setMethod("getName", signature = c("covariate"), definition = function(x) {
  return(paste0("COVARIATE [", "NAME=", x@name, "]"))
})

#' 
#' Create a fixed covariate.
#' 
#' @param name covariate name, single character value
#' @param distribution covariate distribution
#' @return a fixed covariate  
#' @export
Covariate <- function(name, distribution) {
  return(new("covariate", name=name, distribution=toExplicitDistribution(distribution)))
}

#_______________________________________________________________________________
#----                         event_covariate class                         ----
#_______________________________________________________________________________

#' 
#' Event covariate class.
#' 
#' @export
setClass(
  "event_covariate",
  representation(
  ),
  contains="covariate"
)

#' 
#' Create an event covariate. These covariates can be modified further in the
#' interruption events.
#' 
#' @param name covariate name, character
#' @param distribution covariate distribution at time 0
#' @return a time-varying covariate  
#' @export
EventCovariate <- function(name, distribution) {
  return(new("event_covariate", name=name, distribution=toExplicitDistribution(distribution)))
}

#_______________________________________________________________________________
#----                         time_varying_covariate class                  ----
#_______________________________________________________________________________

#' 
#' Time-varying covariate class.
#' 
#' @export
setClass(
  "time_varying_covariate",
  representation(
    table="data.frame"
  ),
  contains="covariate"
)

#' 
#' Create a time-varying covariate.
#' 
#' @param name covariate name, character
#' @param table data.frame, must contain the mandatory columns 'TIME' and 'VALUE'.
#'  An 'ID' column may also be specified. In that case, ID's between 1 and the
#'  max number of subjects in the dataset/arm can be used.
#' @return a time-varying covariate  
#' @export
TimeVaryingCovariate <- function(name, table) {
  tableT0 <- table %>% dplyr::filter(TIME==0)
  tableAfterT0 <- table %>% dplyr::filter(TIME>0)
  hasID <- "ID" %in% colnames(tableT0)
  if (hasID) {
    if (!all(tableT0$ID==seq_len(max(tableT0$ID)))) {
      stop("IDs at time 0 must all be set")
    }
  } else {
    if (nrow(tableT0) == 0) {
      stop("Please provide a value for time 0")
    }
    if (nrow(tableT0) > 1) {
      stop("Only 1 value for time 0 is accepted")
    }
  }
  
  return(new("time_varying_covariate", name=name,
             distribution=toExplicitDistribution(tableT0$VALUE), table=tableAfterT0))
}

#' Merge time-varying covariates into a single data frame. This last data frame
#' will be merged afterwards with all treatment and observation rows.
#' 
#' @param covariates covariates, only time-varying covariates will be extracted
#' @param ids simulation ID's
#' @return a data.frame
#' @importFrom campsismod select
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_df
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' 
mergeTimeVaryingCovariates <- function(covariates, ids) {
  startingID <- min(ids)
  timeVaryingCovariates <- covariates %>% campsismod::select("time_varying_covariate")
  tables <- timeVaryingCovariates %>% .@list %>%
    purrr::map_df(.f=function(covariate) {
      table <- covariate@table %>% dplyr::mutate(VARIABLE=covariate@name)
      if (("ID" %in% colnames(table))) {
        return(table %>% dplyr::mutate(ID=ID + startingID - 1))
      } else {
        retValue <- ids %>% purrr::map_df(.f=function(id) {
          return(cbind(ID=id, table))
        })
        return(retValue)
      }
    })
  return(dplyr::bind_rows(tables) %>% tidyr::pivot_wider(id_cols=c("ID", "TIME"),
                                                         names_from="VARIABLE", values_from="VALUE"))
}

#' Sample time-varying covariates.
#' 
#' @param object time-varying covariates, data.frame form
#' @param armID treatment arm ID
#' @param needsDV append extra column DV, logical value
#' @return a data.frame
#' @importFrom tibble add_column tibble
#' @keywords internal
#' 
sampleTimeVaryingCovariates <- function(object, armID, needsDV) {
  covNames <- colnames(object)
  covNames <- covNames[!(covNames %in% c("ID", "TIME"))]
  
  retValue <- tibble::tibble(
    ID=object$ID, ARM=as.integer(armID), TIME=object$TIME,
    EVID=as.integer(2), MDV=as.integer(0), AMT=as.numeric(0), CMT=as.integer(-1), RATE=as.numeric(0), DOSENO=as.integer(NA),
    IS_INFUSION=as.logical(NA), EVENT_RELATED=as.integer(0)
  )
  if (needsDV) {
    retValue <- retValue %>% tibble::add_column(DV=as.numeric(0), .before="IS_INFUSION")
  }
  
  retValue <- cbind(retValue, object[, covNames])
  return(retValue)
}

#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

#' @rdname sample
setMethod("sample", signature = c("covariate", "integer"), definition = function(object, n) {
  object@distribution <- object@distribution %>% sample(n)
  return(object)
})
