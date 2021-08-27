
#' Pre-process events.
#'
#' @param events interruption events
#' @keywords internal
#' 
preprocessEvents <- function(events) {
  if (is.null(events)) {
    return(Events())
  } else {
    return(events)
  }
}

#' Pre-process function argument.
#'
#' @param fun function or lambda formula
#' @param name function name
#' @return a function in any case
#' @importFrom assertthat assert_that
#' @importFrom plyr is.formula
#' @importFrom rlang as_function
#' @keywords internal
#' 
preprocessFunction <- function(fun, name) {
  if (is.null(fun)) {
    fun <- function(x){x}
    return(fun)
  } else {
    assertthat::assert_that(is.function(fun) || plyr::is.formula(fun),
                            msg=paste0(name, " must be a function or a lambda formula"))
    if (plyr::is.formula(fun)) {
      fun <- rlang::as_function(fun)
      # Class of fun is c("rlang_lambda_function","function")
      # However, not accepted as argument if method signature is "function"... Bug?
      # Workaround is to set a unique class
      class(fun) <- "function"
    }
    return(fun)
  }
}

#' Preprocess 'outvars' argument. Outvars is a character vector which tells
#' pmxsim the mandatory columns to keep in the output dataframe.
#'
#' @param outvars character vector or function
#' @return outvars
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessOutvars <- function(outvars) {
  if (is.null(outvars)) {
    return(character(0))
  } else {
    assertthat::assert_that(is.character(outvars), 
                            msg="outvars must be a character vector with the column names to keep")
    
    # In any cases, we should never see these special variables
    outvars <- outvars[!(outvars %in% c("id", "time", "ARM"))]
    return(outvars)
  }
}

#' Preprocess 'replicates' argument.
#' 
#' @param replicates number of replicates
#' @return same number, but as integer
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessReplicates <- function(replicates) {
  assertthat::assert_that(is.numeric(replicates) && replicates%%1==0 && replicates > 0,
                          msg="replicates not a positive integer")
  return(as.integer(replicates))
}

#' Preprocess 'nocb' argument.
#' 
#' @param nocb nocb argument, logical value
#' @param dest destination engine
#' @return user value, if not specified, return TRUE for mrgsolve and FALSE for RxODE
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessNocb <- function(nocb, dest) {
  if (is.null(nocb)) {
    if (dest=="mrgsolve") {
      nocb <- TRUE
    } else {
      nocb <- FALSE
    }
  }
  assertthat::assert_that(is.logical(nocb) && nocb %>% length()==1 && !is.na(nocb),
                          msg="nocb not a logical value TRUE/FALSE")
  return(nocb)
}

#' Preprocess 'dosing' argument.
#' 
#' @param dosing dosing argument, logical value
#' @return user value, if not specified, return FALSE (observations only)
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessDosing <- function(dosing) {
  if (is.null(dosing)) {
    dosing <- FALSE
  }
  assertthat::assert_that(is.logical(dosing) && dosing %>% length()==1 && !is.na(dosing),
                          msg="dosing not a logical value TRUE/FALSE")
  return(dosing)
}

#' Preprocess subjects ID's.
#' 
#' @param dataset current dataset, data frame form
#' @return list of consecutive ID's
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessIds <- function(dataset) {
  ids <- unique(dataset$ID)
  maxID <- max(ids)
  assertthat::assert_that(all(ids==seq_len(maxID)), msg="ID's must be consecutive numbers, starting at 1")
  return(ids)
}

#' Preprocess ARM column. Add ARM equation in model automatically.
#' 
#' @param dataset current dataset, data frame form
#' @param model model
#' @return updated model
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessArmColumn <- function(dataset, model) {
  if ("ARM" %in% colnames(dataset)) {
    pkRecord <- model@model %>% getByName("MAIN")
    pkRecord <- pkRecord %>% add(Equation("ARM", "ARM"))
    model@model <- model@model %>% replace(pkRecord)
  }
  if ("EVENT_RELATED" %in% colnames(dataset)) {
    pkRecord <- model@model %>% getByName("MAIN")
    pkRecord <- pkRecord %>% add(Equation("EVENT_RELATED", "EVENT_RELATED"))
    model@model <- model@model %>% replace(pkRecord)
  }
  return(model)
}

#' Preprocess 'slices' argument.
#' 
#' @param slices slices argument corresponding to the number of subjects simulated at once
#' @return slices if not NULL, otherwise total number of subjects
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
preprocessSlices <- function(slices, maxID) {
  if (is.null(slices)) {
    return(maxID)
  } else {
    assertthat::assert_that(is.numeric(slices) && slices%%1==0 && slices > 0,
                            msg="slices not a positive integer")
    return(slices)
  }
}

#' Return the 'DROP_OTHERS' string that may be used in the 'outvars' vector for
#' RxODE/mrgsolve to drop all others variables that are usually output in the resulting data frame.
#' 
#' @return a character value
#' @keywords internal
#' 
dropOthers <- function() {
  return("DROP_OTHERS")
}

#' Process 'DROP_OTHERS'.
#'
#' @param x the current data frame
#' @param outvars variables to keep
#' @param dropOthers logical value
#' @return processed data frame
#' @keywords internal
#' 
processDropOthers <- function(x, outvars=character(0), dropOthers) {
  if (!dropOthers) {
    return(x)
  }
  outvars_ <- outvars[!(outvars %in% dropOthers())]
  out <- c("id", "time", "ARM", "EVENT_RELATED", outvars_)
  names <- colnames(x)
  return(x[, names[names %in% out]])
}