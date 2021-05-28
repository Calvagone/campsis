#_______________________________________________________________________________
#----                       time_entry class                                ----
#_______________________________________________________________________________

checkTimeEntry <- function(object) {
  check1 <- expectOne(object, "time")
  check2 <- expectPositiveTimes(object@time)
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