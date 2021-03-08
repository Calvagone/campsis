#_______________________________________________________________________________
#----                         pmx_element class                             ----
#_______________________________________________________________________________

#' 
#' PMX element class.
#' 
#' @export
setClass(
  "pmx_element",
  representation(
  )
)

#_______________________________________________________________________________
#----                           getName                                     ----
#_______________________________________________________________________________

#' Get element name.
#' 
#' @param x element to know the name
#' @return the name of this element
#' @export
getName <- function(x) {
  stop("No default function is provided")
}

setGeneric("getName", function(x) {
  standardGeneric("getName")
})