
#_______________________________________________________________________________
#----                              sample                                   ----
#_______________________________________________________________________________

setMethod("sample", signature = c("pmx_model", "integer"), definition = function(object, n) {
  
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
    }) %>% setNames(colnames(varcov))
    
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
        model@parameters <- model@parameters %>% pmxmod::replace(originalParam)
      }
      retValue[[repIndex]] <- model
    }
  }
  return(retValue)
})