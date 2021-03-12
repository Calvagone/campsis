
#' Process extra arguments.
#' 
#' @param args arguments list
#' @param name argument name to retrieve
#' @param default default value if argument is not present
#' @param mandatory mandatory argument, logical value
#' @return requested argument value
#' @export
processExtraArg <- function(args, name, default=NULL, mandatory=FALSE) {
  if (hasName(args, name)) {
    retValue <- args[[name]]
  } else {
    if (is.null(default) && mandatory) {
      stop(paste0("Extra argument '", name, "' is mandatory."))
    }
    retValue <- default
  }
  return(retValue)
}