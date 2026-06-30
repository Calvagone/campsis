#_______________________________________________________________________________
#----                              outfun class                             ----
#_______________________________________________________________________________

#'
#' Output function class.
#'
#' @slot fun function or purrr-style lambda formula, first argument 'x' must be the results
#' @slot name name of the output function
#' @slot args extra arguments, named list
#' @slot packages packages that must be loaded to execute the given function, character vector
#' @slot level either 'scenario' or 'replicate'. Default is 'scenario'.
#' @slot cls resulting S3 class(es) of the Campsis output
#' @slot outfun_as_arg logical, whether to pass the outfun object as an argument to the function
#' @export
setClass(
  "outfun",
  representation(
    fun = "function",
    name = "character",
    args = "list",
    packages = "character",
    level = "character",
    cls = "character",
    outfun_as_arg = "logical"
  ),
  contains = "pmx_element",
  prototype = prototype(
    fun = function(x, ...) {x}, # Identity function
    name = "default",
    args = list(),
    packages = character(0),
    level = "replicate",
    cls = "campsis_tbl",
    outfun_as_arg = FALSE
  )
)

setMethod("getName", signature = c("outfun"), definition = function(x) {
  return(x@name)
})

#'
#' Create a new output function
#'
#' @param fun function or purrr-style lambda formula, first argument 'x' must be the results
#' @param args extra arguments, named list
#' @param packages packages that must be loaded to execute the given function, character vector
#' @param level where to apply the output function, only 'replicate' is allowed since Campsis v1.9.0
#' @param name name of the output function. Default is 'custom'.
#' @importFrom rlang as_function is_formula
#' @return an output function
#' @export
Outfun <- function(fun = function(x, ...) {x}, args = list(), packages = NULL, level = "replicate", name = "custom") {
  if (is.function(fun)) {
    # Do nothing
  } else if (rlang::is_formula(fun)) {
    fun <- rlang::as_function(fun)
    class(fun) <- "function" # Cast needed to work with S4 class system
  } else {
    stop("fun must be a function or a purrr-style lambda formula")
  }
  assertthat::assert_that(
    level %in% c("replicate"),
    msg = "No level other than 'replicate' is allowed since Campsis v1.9.0"
  )
  if (is.null(packages)) {
    packages <- character(0)
  }

  return(new(
    "outfun",
    fun = fun,
    name = name,
    args = args,
    packages = packages,
    level = level,
    cls = c("custom_campsis_tbl", "campsis_tbl")
  ))
}

applyOutfun <- function(x, outfun, level, ...) {
  assertthat::assert_that(is(outfun, "outfun"), msg = "x is not an output function")

  if (level == outfun@level) {
    # Retrieve all formal arguments of the user-given function
    formalArgs_ <- formalArgs(outfun@fun)

    # Prepare list of arguments
    args <- list(x) %>% # First argument is the Campsis results
      append(outfun@args) # user-given arguments list

    if (outfun@outfun_as_arg) {
      args <- append(args, list(obj = outfun))
    }

    # Some more arguments (like 'replicate' or 'scenario') are transmitted by Campsis automatically
    # This requires the three dot ellipsis to be there
    # Note that if lambda was passed in 'Outfun' constructor, three dot ellipsis is always there
    if ("..." %in% formalArgs_) {
      args <- args %>%
        append(list(...))
    }

    # Load packages
    lapply(outfun@packages, require, character.only = TRUE)

    # Call output function with args
    x <- do.call(outfun@fun, args = args)
  }
  return(x)
}

#_______________________________________________________________________________
#----                         default_outfun class                          ----
#_______________________________________________________________________________

#'
#' Default output function class.
#' @export
setClass(
  "default_outfun",
  representation(
  ),
  contains = "outfun",
  prototype = prototype(
    fun = function(x, ...) {x}, # Identity function
    name = "default",
    cls = c("std_campsis_tbl", "campsis_tbl")
  )
)

#'
#' Default output function (identity function).
#' @return an output function that returns the Campsis results as is.
#' @export
DefaultOutfun <- function() {
  return(new("default_outfun"))
}

setMethod("loadFromJSON", signature = c("default_outfun", "json_element"), definition = function(object, json) {
    return(object) # Nothing to do
  }
)

#_______________________________________________________________________________
#----                            pi_outfun class                            ----
#_______________________________________________________________________________

pi_wrapper <- function(x, obj, ...) {
  compute_pi(
    x = x,
    variable = obj@variable,
    strata = obj@strata,
    level = obj@pi_level
  )
}

#'
#' Prediction interval output function class.
#'
#' @slot variable variable(s) used to compute the prediction interval, character vector
#' @slot strata named vector with the strata to use
#' @slot pi_level PI level, default is 0.9 (90\% PI)
#' @export
setClass(
  "pi_outfun",
  representation(
    variable = "character",
    strata = "vector",
    pi_level = "numeric"
  ),
  contains = "outfun",
  prototype = prototype(
    fun = pi_wrapper,
    variable = character(0),
    strata = getDefaultStrata(),
    pi_level = 0.90,
    name = "default_pi",
    cls = c("pi_campsis_tbl", "campsis_tbl"),
    outfun_as_arg = TRUE
  )
)

#'
#' Create a prediction interval output function
#'
#' @param variable variable(s) used to compute the prediction interval, character vector
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all")
#' @param level PI level, default is 0.9 (90\% PI)
#' @param name name of the output function. Default is 'pi_<variable>_<level>pc'.
#' @importFrom assertthat assert_that
#' @return a pi_outfun object
#' @export
PIOutfun <- function(variable, strata = getDefaultStrata(), level = 0.9,
  name = sprintf("pi_%s_%i%%", paste0(variable, collapse = "_"), round(level * 100))) {
  assertthat::assert_that(
    is.character(variable) && length(variable) >= 1,
    msg = "variable must be a non-empty character vector"
  )
  assertthat::assert_that(
    is.null(strata) ||
      (is.atomic(strata) &&
        !is.null(names(strata)) &&
        all(nzchar(names(strata)))),
    msg = "strata must be a fully named vector or NULL"
  )
  assertthat::assert_that(
    is.numeric(level) && level > 0 && level < 1,
    msg = "level must be a numeric value between 0 and 1"
  )

  return(new(
    "pi_outfun",
    name = name,
    variable = variable,
    strata = strata,
    pi_level = level
  ))
}

setMethod("loadFromJSON", signature = c("pi_outfun", "json_element"), definition = function(object, json) {
    object <- campsismod::mapJSONPropertiesToS4Slots(object, json)
    return(object)
  }
)

#_______________________________________________________________________________
#----                          stats_outfun class                           ----
#_______________________________________________________________________________

stats_wrapper <- function(x, obj, ...) {
  compute_stats(
    x = x,
    variable = obj@variable,
    strata = obj@strata,
    stats = obj@stats
  )
}

#'
#' Statistics output function class.
#'
#' @slot variable variable(s) used to compute the statistics, character vector
#' @slot strata named vector with the strata to use
#' @slot stats character vector of statistics to compute
#' @export
setClass(
  "stats_outfun",
  representation(
    variable = "character",
    strata = "vector",
    stats = "character"
  ),
  contains = "outfun",
  prototype = prototype(
    fun = stats_wrapper,
    variable = character(0),
    strata = getDefaultStrata(),
    stats = c("p5", "median", "p95"),
    name = "default_stats",
    cls = c("stats_campsis_tbl", "campsis_tbl"),
    outfun_as_arg = TRUE
  )
)

#'
#' Create a statistics output function
#'
#' @param variable variable(s) used to compute the statistics, character vector
#' @param strata named vector with the strata to use, default is c(SCENARIO="all", ARM="all")
#' @param stats character vector of statistics to compute. Supported: "median", "mean", or percentiles like "p5", "p95". Default is c("p5", "median", "p95").
#' @param name name of the output function. Default is 'stats_<variable>'.
#' @importFrom assertthat assert_that
#' @return a stats_outfun object
#' @export
StatsOutfun <- function(variable, strata = getDefaultStrata(), stats = c("p5", "median", "p95"),
  name = sprintf("stats_%s", paste0(variable, collapse = "_"))) {
  assertthat::assert_that(
    is.character(variable) && length(variable) >= 1,
    msg = "variable must be a non-empty character vector"
  )
  assertthat::assert_that(
    is.null(strata) ||
      (is.atomic(strata) &&
        !is.null(names(strata)) &&
        all(nzchar(names(strata)))),
    msg = "strata must be a fully named vector or NULL"
  )
  assertthat::assert_that(
    is.character(stats) && length(stats) >= 1,
    msg = "stats must be a non-empty character vector"
  )

  return(new(
    "stats_outfun",
    name = name,
    variable = variable,
    strata = strata,
    stats = stats
  ))
}

setMethod("loadFromJSON", signature = c("stats_outfun", "json_element"), definition = function(object, json) {
    object <- campsismod::mapJSONPropertiesToS4Slots(object, json)
    return(object)
  }
)


#_______________________________________________________________________________
#----                          nca_table_outfun class                       ----
#_______________________________________________________________________________

nca_table_wrapper <- function(x, obj, ...) {
  table <- obj@table
  eval_str <- sprintf("table |> campsisnca::calculate(x=x) |> export(dest='dataframe', type='%s')", obj@export_type)
  export_df <- eval(parse(text = eval_str))
  return(export_df)
}

#'
#' NCA table output function class.
#'
#' @slot table Campsisnca table object
#' @slot export_type type of export, 'summary', 'summary_wide', 'summary_pretty', 'individual' or 'individual_wide'
#' @export
setClass(
  "nca_table_outfun",
  representation(
    table = "ANY",
    export_type = "character"
  ),
  contains = "outfun",
  prototype = prototype(
    fun = nca_table_wrapper,
    name = "default_nca_table",
    cls = c("nca_table_campsis_tbl", "campsis_tbl"),
    packages = c("campsisnca", "gtsummary"),
    outfun_as_arg = TRUE
  )
)

open_nca_table <- function(json) {
  table <- json
  eval_table_str <- "campsisnca::NCATable(json=table)"
  return(eval(parse(text = eval_table_str)))
}

#'
#' Create a NCA table output function
#'
#' @param table NCA table from campsisnca, object, path to JSON, or JSON element
#' @param export_type type of export, 'summary', 'summary_wide', 'summary_pretty', 'individual' or 'individual_wide'
#' @param name name of the output function. Default is 'default_nca_table'.
#' @importFrom assertthat assert_that
#' @return a stats_outfun object
#' @export
NCATableOutfun <- function(table, export_type = "summary", name = "default_nca_table") {
  table <- open_nca_table(json=table)
  return(new(
    "nca_table_outfun",
    name = name,
    table = table,
    export_type = export_type
  ))
}

setMethod("loadFromJSON", signature = c("nca_table_outfun", "json_element"), definition = function(object, json) {
    table <- open_nca_table(json=json@data$table)
    json@data$table <- NULL
    object <- campsismod::mapJSONPropertiesToS4Slots(object, json)
    object@table <- table
    return(object)
}
)
