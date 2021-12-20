#_______________________________________________________________________________
#----                        treatment_iov class                            ----
#_______________________________________________________________________________

validateTreatmentIOV <- function(object) {
  check1 <- expectOneForAll(object, c("colname", "distribution"))
  check2 <- expectZeroOrMore(object, "dose_numbers")
  return(c(check1, check2))
}

#' 
#' Treatment IOV class.
#' 
#' @slot colname name of the column that will be output in dataset
#' @slot distribution distribution
#' @slot dose_numbers associated dose numbers, integer vector, same length as values
#' @export
setClass(
  "treatment_iov",
  representation(
    colname = "character",
    distribution = "distribution",
    dose_numbers = "integer"
  ),
  contains="pmx_element",
  validity=validateTreatmentIOV 
)

#'
#' Define inter-occasion variability (IOV) into the dataset. A new variable of name
#' 'colname' will be output into the dataset and will vary at each dose number
#' according to the given distribution.
#'
#' @param colname name of the column that will be output in dataset
#' @param distribution distribution
#' @param doseNumbers dose numbers, if provided, IOV is generated at these doses only. By default, IOV is generated for all doses.
#' @return IOV
#' @export
IOV <- function(colname, distribution, doseNumbers=NULL) {
  if (is.null(doseNumbers)) {
    doseNumbers <- integer(0)
  }
  return(new("treatment_iov", colname=colname, distribution=toExplicitDistribution(distribution),
             dose_numbers=as.integer(doseNumbers) %>% unique() %>% base::sort()))
}

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature = c("treatment_iov"), definition = function(x) {
  return(x@colname)
})
