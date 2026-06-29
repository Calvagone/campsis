#' 
#' Default settings class.
#' 
#' @slot engine simulation engine, character
#' @slot seed random seed number, integer
#' @slot outvars output variables, character vector
#' @slot disabled_variabilities variabilities to disable in the simulation, character vector
#' @slot dosing output dosing information, logical
#' @export
setClass(
  "default_settings",
  representation(
    engine="character",
    seed="integer", # NA means 'AUTO'
    outvars="character",
    outfuns="outfuns",
    disabled_variabilities="character",
    dosing="logical"
  ),
  prototype=prototype(engine="rxode2", seed=as.integer(NA), outvars=character(),
                      outfuns=Outfuns(), disabled_variabilities=character(), dosing=FALSE)
)

#'
#' Create default settings.
#'
#' @param engine simulation engine, character
#' @param seed random seed number, integer (or NULL for auto-generated seed)
#' @param outvars output variables, character vector
#' @param outfuns output functions, outfuns object
#' @param disabled_variabilities variabilities to disable in the simulation, character vector
#' @param dosing output dosing information, logical
#' @return default settings
#' @export
DefaultSettings <- function(engine = "rxode2", seed = NULL, outvars = character(),
 outfuns=Outfuns(), disabled_variabilities = character(), dosing = FALSE) {
  if (is.null(seed)) {
    seed <- as.integer(NA)
  }
  return(new(
    "default_settings",
    engine = engine,
    seed = as.integer(seed),
    outvars = outvars,
    outfuns = outfuns,
    disabled_variabilities = disabled_variabilities,
    dosing = dosing
  ))
}

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("default_settings", "json_element"), definition=function(object, json) {
  json_outfuns <- json@data$outfuns
  json@data$outfuns <- NULL
  object <- campsismod::mapJSONPropertiesToS4Slots(object, json)

  if (is.null(json_outfuns)) {
    object@outfuns <- Outfuns()
  } else {
    object@outfuns <- loadFromJSON(Outfuns(), JSONElement(json_outfuns))
  }
  
  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("default_settings"), definition=function(object) {
  if (identical(object, DefaultSettings())) {
    cat("Default arguments: default")    
  } else {
    # Collect outfun names
    outfun_names <- character(0)
    if (length(object@outfuns) > 0) {
      outfun_names <- vapply(object@outfuns@list, function(x) x@name, character(1))
    }
    outfuns_str <- paste0(paste0("'", outfun_names, "'"), collapse=", ")
    
    cat(sprintf("Default arguments: engine='%s', seed=%s, outvars=[%s], outfuns=[%s], disabled_variabilities=[%s], dosing=%s",
        object@engine, as.character(object@seed), paste0(paste0("'", object@outvars, "'"), collapse=", "),
        outfuns_str, paste0(paste0("'", object@disabled_variabilities, "'"), collapse=", "), object@dosing)) 
  }
  cat("\n")
})
