#_______________________________________________________________________________
#----                         protocol class                                ----
#_______________________________________________________________________________

#' 
#' Protocol class.
#' 
#' @export
setClass(
  "protocol",
  representation(
    treatment = "treatment",
    observations = "observations_set"
  ),
  prototype=prototype(treatment=new("treatment"), observations=new("observations_set"))
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

setMethod("add", signature=c("protocol", "treatment_entry"), definition=function(object, x) {
  return(object@treatment %>% add(x))
})

setMethod("add", signature=c("protocol", "observations"), definition=function(object, x) {
  return(object@observations %>% add(x))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("protocol"), definition=function(object) {
  cat("Protocol:")
  cat("\n")
  
  doseTimes <- get_times(object@treatment)
  object@observations@list <- object@observations@list %>%
    purrr::map(.f=function(x) {
      if (is(x@rep, "dosing_schedule")) {
        x@rep <- RepeatAtSchedule(doseTimes)
      }
      return(x)
    })
  show(object@treatment)
  show(object@observations)
})
