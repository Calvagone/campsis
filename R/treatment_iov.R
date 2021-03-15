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
#' @param compartment compartment number
#' @param distribution distribution
#' @return IOV
#' @export
IOV <- function(colname, distribution) {
  if (is(distribution, "parameter_distribution")) {
    stop("IOV distribution can't be a parameter distribution. Do you want to define it as an eta distribution instead?")
  }
  return(new("treatment_iov", colname=colname, distribution=distribution))
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
