#_______________________________________________________________________________
#----                           occasion class                              ----
#_______________________________________________________________________________

validateOccasion <- function(object) {
  check1 <- expectOne(object, "colname")
  check2 <- expectOneOrMore(object, "values")
  check3 <- expectOneOrMore(object, "dose_numbers")
  check4 <-
    if (object@values %>% length() == object@dose_numbers %>% length()) {
      character()
    } else {
      "values and dose_numbers slots should have equal lengths"
    }
  return(c(check1, check2, check3, check4))
}

#' 
#' Occasion class.
#' 
#' @export
setClass(
  "occasion",
  representation(
    colname = "character",
    values = "integer",
    dose_numbers = "integer"
  ),
  contains="pmx_element",
  validity=validateOccasion 
)

#'
#' Create an occasion mapping. Occasions can only be defined at treatment times, via
#' the dose numbers.
#'
#' @param colname name of the column that will be output in dataset
#' @param values the occasion numbers, any integer vector
#' @param doseNumbers the related dose numbers, any integer vector of same length as 'values'
#' @return occasion object
#' @export
Occasion <- function(colname, values, doseNumbers) {
  return(new("occasion", colname=colname, values=as.integer(values), dose_numbers=as.integer(doseNumbers)))
}

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature = c("occasion"), definition = function(x) {
  return(paste0("OCCASION [", "COLNAME=", x@colname, "]"))
})

#_______________________________________________________________________________
#----                         getColumnName                                 ----
#_______________________________________________________________________________

setMethod("getColumnName", signature = c("occasion"), definition = function(x) {
  return(x@colname)
})
