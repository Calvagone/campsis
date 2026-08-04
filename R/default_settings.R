#'
#' Default settings class.
#'
#' @slot engine Simulation engine: \code{'rxode2'} or \code{'mrgsolve'}.
#'   Default is \code{NULL} (unspecified). If \code{NULL}, \code{'mrgsolve'} is used first 
#'   (if installed), followed by \code{'rxode2'} (if installed).
#' @slot seed random seed number, integer
#' @slot replicates number of replicates, integer
#' @slot outvars output variables, character vector
#' @slot outfuns output functions, outfuns object
#' @slot disabled_variabilities variabilities to disable in the simulation, character vector
#' @slot dosing output dosing information, logical
#' @export
setClass(
  "default_settings",
  representation(
    engine = "character", # NA means
    seed = "integer", # NA means 'AUTO'
    replicates = "integer",
    outvars = "character",
    outfuns = "outfuns",
    disabled_variabilities = "character",
    dosing = "logical"
  ),
  prototype = prototype(
    engine = as.character(NA),
    seed = as.integer(NA),
    replicates = 1L,
    outvars = character(),
    outfuns = Outfuns(),
    disabled_variabilities = character(),
    dosing = FALSE
  )
)

#'
#' Create default settings.
#'
#' @param engine Simulation engine: \code{'rxode2'} or \code{'mrgsolve'}.
#'   Default is \code{NULL} (unspecified). If \code{NULL}, \code{'mrgsolve'} is used first 
#'   (if installed), followed by \code{'rxode2'} (if installed).
#' @param seed random seed number, integer (or NULL for auto-generated seed)
#' @param replicates number of replicates, integer. Default is 1.
#' @param outvars output variables, character vector
#' @param outfuns output functions, outfuns object
#' @param disabled_variabilities variabilities to disable in the simulation, character vector
#' @param dosing output dosing information, logical
#' @return default settings
#' @export
DefaultSettings <- function(
  engine = NULL,
  seed = NULL,
  replicates = 1L,
  outvars = character(),
  outfuns = Outfuns(),
  disabled_variabilities = character(),
  dosing = FALSE
) {
  if (is.null(engine)) {
    engine <- as.character(NA)
  }
  if (is.null(seed)) {
    seed <- as.integer(NA)
  }
  return(new(
    "default_settings",
    engine = as.character(engine),
    seed = as.integer(seed),
    replicates = as.integer(replicates),
    outvars = outvars,
    outfuns = outfuns,
    disabled_variabilities = disabled_variabilities,
    dosing = dosing
  ))
}

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("default_settings", "json_element"), definition = function(object, json) {
  json_outfuns <- json@data$outfuns
  json@data$outfuns <- NULL
  object <- campsismod::map_json_properties_to_s4_slots(object, json)

  # replicates is optional in JSON; coerce to integer (JSON numbers are numeric)
  object@replicates <- as.integer(object@replicates)

  if (is.null(json_outfuns)) {
    object@outfuns <- Outfuns()
  } else {
    object@outfuns <- load_from_json(Outfuns(), JSONElement(json_outfuns))
  }

  return(object)
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature = c("default_settings"), definition = function(object) {
  if (identical(object, DefaultSettings())) {
    cat("Default arguments: default")
  } else {
    # Prepare outfuns string for display
    outfuns_str <- ""
    if (length(object@outfuns) > 0) {
      outfun_names <- object@outfuns@list %>% purrr::map_chr(~ .x@name)
      outfuns_str <- paste0(paste0("'", outfun_names, "'"), collapse = ", ")
    }
    # Prepare disabled_variabilities string for display
    disabled_variabilities_str <- ""
    if (length(object@disabled_variabilities) > 0) {
      disabled_variabilities_str <- paste0(
        paste0("'", object@disabled_variabilities, "'"),
        collapse = ", "
      )
    }

    cat(sprintf(
      "Default arguments: engine='%s', seed=%s, replicates=%s, outvars=[%s], outfuns=[%s], disabled_variabilities=[%s], dosing=%s",
      object@engine,
      as.character(object@seed),
      as.character(object@replicates),
      paste0(paste0("'", object@outvars, "'"), collapse = ", "),
      outfuns_str,
      disabled_variabilities_str,
      object@dosing
    ))
  }
  cat("\n")
})
