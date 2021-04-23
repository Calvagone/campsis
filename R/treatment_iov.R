#_______________________________________________________________________________
#----                        treatment_iov class                            ----
#_______________________________________________________________________________

validateTreatmentIOV <- function(object) {
  return(expectOneForAll(object, c("colname", "distribution")))
}

#' 
#' Treatment IOV class.
#' 
#' @export
setClass(
  "treatment_iov",
  representation(
    colname = "character",
    distribution = "distribution"
  ),
  contains="pmx_element",
  validity=validateTreatmentIOV 
)

#'
#' Create IOV.
#'
#' @param colname name of the column that will be output in dataset
#' @param distribution distribution
#' @return IOV
#' @export
IOV <- function(colname, distribution) {
  return(new("treatment_iov", colname=colname, distribution=toExplicitDistribution(distribution)))
}

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature = c("treatment_iov"), definition = function(x) {
  return(paste0("IOV [", "COLNAME=", x@colname, "]"))
})

#_______________________________________________________________________________
#----                         getColumnName                                 ----
#_______________________________________________________________________________

setMethod("getColumnName", signature = c("treatment_iov"), definition = function(x) {
  return(x@colname)
})
