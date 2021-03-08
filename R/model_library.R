
#' Get NONMEM model template.
#' 
#' @param advan ADVAN number
#' @param trans TRANS number
#' @return a PMX model ready to be used
#' @export
getNONMEMModelTemplate <- function(advan, trans) {
  return(model_library[[paste0("advan", advan, "_trans", trans)]])
}
