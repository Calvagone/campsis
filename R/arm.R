
#_______________________________________________________________________________
#----                            arm class                                  ----
#_______________________________________________________________________________

checkArm <- function(object) {
  return(checkObject(object, c("id", "subjects")))
}

#' 
#' Arm class.
#' 
#' @export
setClass(
  "arm",
  representation(
    id = "integer",
    subjects = "integer"
  ),
  prototype=prototype(id=as.integer(1), subjects=as.integer(1))
)

