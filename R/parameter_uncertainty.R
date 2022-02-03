
#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

#' @rdname sample
#' @importFrom stats setNames
#' @importFrom MASS mvrnorm
#' @importFrom purrr accumulate map map_dbl pluck
setMethod("sample", signature = c("campsis_model", "integer"), definition = function(object, n) {
  
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
      # .x is the accumulating value
      # .y is element in the list
      omegas <- model@parameters %>% select("omega")
      if (omegas %>% length() > 1) {
        purrr::accumulate(.x=omegas@list, .f=function(.x, .y) {
            if (isTRUE(.y@same)) {
              if (is.na(.x@same)) {
                stop("Inconsistent same column. Slot 'same' of Previous OMEGA can't be NA.")
              }
              # Take value just above
              .y@value <- .x@value
              model@parameters <<- model@parameters %>% replace(.y)
            }
            return(.y)
          },
          .init = omegas@list[[1]]
        )
      }
      
      # Store model in list
      retValue[[repIndex]] <- model
    }
  }

  return(retValue)
})
