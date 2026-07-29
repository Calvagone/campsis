#_______________________________________________________________________________
#----                       time_entry class                                ----
#_______________________________________________________________________________

checkTimeEntry <- function(object) {
  check1 <- expect_one_or_more(object, "time")
  check2 <- expect_positive_values(object, "time")
  return(c(check1, check2))
}

setClass(
  "time_entry",
  representation(
    time = "numeric"
  ),
  contains="pmx_element",
  validity=checkTimeEntry
)
