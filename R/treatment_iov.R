#_______________________________________________________________________________
#----                        treatment_iov class                            ----
#_______________________________________________________________________________

validate_treatment_iov <- function(object) {
  check1 <- expect_one(object, "colname")
  check2 <- expect_zero_or_more(object, "dose_numbers")
  # Two different ways of using IOV:
  # 1) omega_ref is defined, distribution is null/undefined
  # 2) omega_ref is not defined, distribution is defined
  check3 <- TRUE
  is_omega_ref_defined <- !is.na(object@omega_ref)
  is_distribution_defined <- !is(object@distribution, "undefined_distribution")

  if (is_omega_ref_defined && is_distribution_defined) {
    check3 <- "omega_ref and distribution cannot be defined at the same time"
  }
  if (!is_omega_ref_defined && !is_distribution_defined) {
    check3 <- "either omega_ref or distribution must be defined"
  }

  retValue <- c(check1, check2, check3)
  if (is.logical(retValue) && all(retValue)) {
    return(TRUE)
  }
  return(retValue[retValue != TRUE])
}

#'
#' Treatment IOV class.
#'
#' @slot colname name of the column that will be output in dataset
#' @slot distribution distribution
#' @slot dose_numbers associated dose numbers, integer vector, same length as values
#' @slot omega_ref name of the OMEGA (e.g. 'IOV' without the 'OMEGA_')
#' @export
setClass(
  "iov",
  representation(
    colname = "character",
    distribution = "distribution",
    dose_numbers = "integer",
    omega_ref = "character"
  ),
  contains = "pmx_element",
  prototype = prototype(distribution = new("undefined_distribution"), omega_ref = as.character(NA)),
  validity = validate_treatment_iov
)

#'
#' Define inter-occasion variability (IOV) into the dataset. A new variable of name
#' 'colname' will be output into the dataset and will vary at each dose number
#' according to the given distribution.
#'
#' @param colname name of the column that will be output in dataset
#' @param distribution distribution
#' @param dose_numbers dose numbers, if provided, IOV is generated at these doses only. By default, IOV is generated for all doses.
#' @param omega_ref name of the OMEGA (e.g. 'IOV' without the 'OMEGA_')
#' @return an IOV object
#' @export
IOV <- function(colname, distribution = NULL, dose_numbers = NULL, omega_ref = NULL) {
  if (is.null(dose_numbers)) {
    dose_numbers <- integer(0)
  }
  if (is.null(distribution)) {
    distribution <- new("undefined_distribution")
  }
  if (is.null(omega_ref)) {
    omega_ref <- as.character(NA)
  }
  return(new(
    "iov",
    colname = trimws(colname),
    distribution = to_explicit_distribution(distribution),
    dose_numbers = as.integer(dose_numbers) %>% unique() %>% base::sort(),
    omega_ref = as.character(omega_ref)
  ))
}

#_______________________________________________________________________________
#----                             get_name                                  ----
#_______________________________________________________________________________

setMethod("get_name", signature = c("iov"), definition = function(x) {
  return(x@colname)
})

#_______________________________________________________________________________
#----                           load_from_json                                ----
#_______________________________________________________________________________

#' @importFrom campsismod map_json_properties_to_s4_slots
setMethod("load_from_json", signature = c("iov", "json_element"), definition = function(object, json) {
  object <- map_json_properties_to_s4_slots(object, json)
  return(object)
})
