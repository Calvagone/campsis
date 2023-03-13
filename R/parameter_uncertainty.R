
#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

#' @rdname sample
#' @importFrom stats setNames
#' @importFrom MASS mvrnorm
#' @importFrom purrr accumulate map map_dbl pluck
#' @importFrom methods validObject
setMethod("sample", signature = c("campsis_model", "integer"), definition = function(object, n) {
  # Validate original Campsis model before sampling parameter uncertainty
  methods::validObject(object, complete=TRUE)
  
  varcov <- object@parameters@varcov
  retValue <- list()

  # No variance-covariance matrix, simply duplicate the model
  if (varcov %>% length() == 0) {
    for (repIndex in seq_len(n)) {
      retValue[[repIndex]] <- object
    }
  } else {
    # Variance-covariance matrix detected, generate parameters
    originalParams <- colnames(varcov) %>% purrr::map(.f=function(.x){
      return(object@parameters %>% getByName(.x)) 
    }) %>% stats::setNames(colnames(varcov))
    
    mean <- originalParams %>% purrr::map_dbl(~.x@value)
    parametersTable <- MASS::mvrnorm(n=n, mu=mean, Sigma=varcov)
    
    # Duplicate and adapt original model
    for (repIndex in seq_len(n)) {
      model <- object
      row <- parametersTable[repIndex, ]
      paramNames <- names(row)
      paramValues <- as.numeric(row)
      
      for (paramIndex in seq_along(paramNames)) {
        # pluck can be used because originalParams is a named list
        originalParam <- originalParams %>% purrr::pluck(paramNames[paramIndex])
        originalParam@value <- paramValues[paramIndex]
        model@parameters <- model@parameters %>% replace(originalParam)
      }

      # Still need to update the omegas 'SAME'
      # .x is the accumulated results or initial value (a 'parameters' object here)
      # .y next value in sequence (an omega here)
      omegas <- model@parameters %>% select("omega")
      if (omegas %>% length() > 1) {
        omegas_ <- Parameters()
        omegas_ <- omegas_ %>% add(omegas@list[[1]])
        
        returned_omega_ <- purrr::accumulate(.x=omegas@list[2:length(omegas@list)], .f=function(.x, .y) {
          lastOmega <- .x@list[[.x@list %>% length()]]
          currentOmega <- .y
          if (isTRUE(currentOmega@same)) {
            if (is.na(lastOmega@same)) {
              stop("Inconsistent same column. Slot 'same' of Previous OMEGA can't be NA.")
            }
            # Take value just above
            currentOmega@value <- lastOmega@value
          }
          
          # Accumulate here
          .x <- .x %>% add(currentOmega)
          
          return(.x)
        }, .init=omegas_)
        
        # Replace all previous omega's by new ones
        model@parameters <- model@parameters %>% replace(returned_omega_)
      }

      # Store model in list
      retValue[[repIndex]] <- model
    }
  }

  return(retValue)
})
