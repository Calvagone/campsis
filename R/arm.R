
#_______________________________________________________________________________
#----                            arm class                                  ----
#_______________________________________________________________________________

checkArm <- function(object) {
  return(expectOneForAll(object, c("id", "subjects")))
}

#' 
#' Arm class.
#' 
#' @export
setClass(
  "arm",
  representation(
    id = "integer",
    subjects = "integer",
    protocol = "protocol",
    covariates = "covariates"
  ),
  prototype=prototype(id=as.integer(1), subjects=as.integer(1), protocol=new("protocol"), covariates=new("covariates"))
)

