#_______________________________________________________________________________
#----                       time_entry class                                ----
#_______________________________________________________________________________

checkTimeEntry <- function(object) {
  return(expectOne(object, "time"))
}

setClass(
  "time_entry",
  representation(
    time = "numeric"
  ),
  validity=checkTimeEntry
)